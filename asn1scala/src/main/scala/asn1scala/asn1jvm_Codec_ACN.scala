package asn1scala

import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}
import stainless.math.{wrapping => wrappingExpr, _}
import stainless.annotation._
import stainless.proof._
import stainless.lang.StaticChecks._

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
   def TODO_ASN1_OTHER_prefixLemma(acn1: ACN, acn2: ACN): Unit = ()

   @ghost @pure @opaque @inlineOnce
   def dec_Int_TwosComplement_ConstSize_8_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.base.bitStream.buf.length == acn2.base.bitStream.buf.length)
      require(acn1.base.bitStream.validate_offset_bits(8))
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         acn1.base.bitStream.bitIndex + 8
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, b1) = acn1.dec_Int_TwosComplement_ConstSize_8_pure()
      val (acn2Res, b2) = acn2Reset.dec_Int_TwosComplement_ConstSize_8_pure()

      {
         BitStream.readBytePrefixLemma(acn1.base.bitStream, acn2.base.bitStream)
      }.ensuring { _ =>
         acn1Res.base.bitStream.bitIndex == acn2Res.base.bitStream.bitIndex && b1 == b2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_8_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.base.bitStream.buf.length == acn2.base.bitStream.buf.length)
      require(acn1.base.bitStream.validate_offset_bits(8))
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         acn1.base.bitStream.bitIndex + 8
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, b1) = acn1.dec_Int_PositiveInteger_ConstSize_8_pure()
      val (acn2Res, b2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_8_pure()

      {
         BitStream.readBytePrefixLemma(acn1.base.bitStream, acn2.base.bitStream)
      }.ensuring { _ =>
         acn1Res.base.bitStream.bitIndex == acn2Res.base.bitStream.bitIndex && b1 == b2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_prefixLemma(acn1: ACN, acn2: ACN, nBits: Int): Unit = {
      require(0 <= nBits && nBits <= 64)
      require(acn1.base.bitStream.buf.length == acn2.base.bitStream.buf.length)
      require(acn1.base.bitStream.validate_offset_bits(nBits))
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         acn1.base.bitStream.bitIndex + nBits
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, l1) = acn1.dec_Int_PositiveInteger_ConstSize_pure(nBits)
      val (acn2Res, l2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_pure(nBits)

      {
         BitStream.readNLSBBitsMSBFirstPrefixLemma(acn1.base.bitStream, acn2.base.bitStream, nBits)
      }.ensuring { _ =>
         acn1Res.base.bitStream.bitIndex == acn2Res.base.bitStream.bitIndex && l1 == l2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_big_endian_16_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.base.bufLength() == acn2.base.bufLength())
      require(BitStream.validate_offset_bits(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit, 16))
      require(BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 16 <= BitStream.bitIndex(acn2.base.bitStream.buf.length, acn2.base.bitStream.currentByte, acn2.base.bitStream.currentBit)) // TODO: Needed?
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 16
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, i1) = acn1.dec_Int_PositiveInteger_ConstSize_big_endian_16_pure()
      val (acn2Res, i2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_big_endian_16_pure()

      {
         val end = (BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) / 8 + 2).toInt
         arrayRangesEqImpliesEq(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, acn1.base.bitStream.currentByte, end)
         arrayRangesEqImpliesEq(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, acn1.base.bitStream.currentByte + 1, end)
         assert(i1 == i2)
      }.ensuring { _ =>
         BitStream.bitIndex(acn1Res.base.bitStream.buf.length, acn1Res.base.bitStream.currentByte, acn1Res.base.bitStream.currentBit) == BitStream.bitIndex(acn2Res.base.bitStream.buf.length, acn2Res.base.bitStream.currentByte, acn2Res.base.bitStream.currentBit) &&& i1 == i2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_big_endian_32_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.base.bufLength() == acn2.base.bufLength())
      require(BitStream.validate_offset_bits(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit, 32))
      require(BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 32 <= BitStream.bitIndex(acn2.base.bitStream.buf.length, acn2.base.bitStream.currentByte, acn2.base.bitStream.currentBit)) // TODO: Needed?
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 32
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, i1) = acn1.dec_Int_PositiveInteger_ConstSize_big_endian_32_pure()
      val (acn2Res, i2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_big_endian_32_pure()

      {
         arrayBitRangesEqSlicedLemma(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 32, 0, BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 16)
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
         BitStream.bitIndex(acn1Res.base.bitStream.buf.length, acn1Res.base.bitStream.currentByte, acn1Res.base.bitStream.currentBit) == BitStream.bitIndex(acn2Res.base.bitStream.buf.length, acn2Res.base.bitStream.currentByte, acn2Res.base.bitStream.currentBit) && i1 == i2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_big_endian_64_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.base.bufLength() == acn2.base.bufLength())
      require(BitStream.validate_offset_bits(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit, 64))
      require(BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 64 <= BitStream.bitIndex(acn2.base.bitStream.buf.length, acn2.base.bitStream.currentByte, acn2.base.bitStream.currentBit)) // TODO: Needed?
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 64
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, i1) = acn1.dec_Int_PositiveInteger_ConstSize_big_endian_64_pure()
      val (acn2Res, i2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_big_endian_64_pure()

      {
         arrayBitRangesEqSlicedLemma(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 64, 0, BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 32)
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
         BitStream.bitIndex(acn1Res.base.bitStream.buf.length, acn1Res.base.bitStream.currentByte, acn1Res.base.bitStream.currentBit) == BitStream.bitIndex(acn2Res.base.bitStream.buf.length, acn2Res.base.bitStream.currentByte, acn2Res.base.bitStream.currentBit) && i1 == i2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_little_endian_16_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.base.bufLength() == acn2.base.bufLength())
      require(BitStream.validate_offset_bits(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit, 16))
      require(BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 16 <= BitStream.bitIndex(acn2.base.bitStream.buf.length, acn2.base.bitStream.currentByte, acn2.base.bitStream.currentBit)) // TODO: Needed?
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 16
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, i1) = acn1.dec_Int_PositiveInteger_ConstSize_little_endian_16_pure()
      val (acn2Res, i2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_little_endian_16_pure()

      {
         val end = (BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) / 8 + 2).toInt
         arrayRangesEqImpliesEq(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, acn1.base.bitStream.currentByte, end)
         arrayRangesEqImpliesEq(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, acn1.base.bitStream.currentByte + 1, end)
      }.ensuring { _ =>
         BitStream.bitIndex(acn1Res.base.bitStream.buf.length, acn1Res.base.bitStream.currentByte, acn1Res.base.bitStream.currentBit) == BitStream.bitIndex(acn2Res.base.bitStream.buf.length, acn2Res.base.bitStream.currentByte, acn2Res.base.bitStream.currentBit) && i1 == i2
      }
   }
   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_little_endian_32_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.base.bufLength() == acn2.base.bufLength())
      require(BitStream.validate_offset_bits(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit, 32))
      require(BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 32 <= BitStream.bitIndex(acn2.base.bitStream.buf.length, acn2.base.bitStream.currentByte, acn2.base.bitStream.currentBit)) // TODO: Needed?
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 32
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, i1) = acn1.dec_Int_PositiveInteger_ConstSize_little_endian_32_pure()
      val (acn2Res, i2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_little_endian_32_pure()

      {
         arrayBitRangesEqSlicedLemma(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 32, 0, BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 16)
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
         BitStream.bitIndex(acn1Res.base.bitStream.buf.length, acn1Res.base.bitStream.currentByte, acn1Res.base.bitStream.currentBit) == BitStream.bitIndex(acn2Res.base.bitStream.buf.length, acn2Res.base.bitStream.currentByte, acn2Res.base.bitStream.currentBit) && i1 == i2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def dec_Int_PositiveInteger_ConstSize_little_endian_64_prefixLemma(acn1: ACN, acn2: ACN): Unit = {
      require(acn1.base.bufLength() == acn2.base.bufLength())
      require(BitStream.validate_offset_bits(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit, 64))
      require(BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 64 <= BitStream.bitIndex(acn2.base.bitStream.buf.length, acn2.base.bitStream.currentByte, acn2.base.bitStream.currentBit)) // TODO: Needed?
      require(arrayBitRangesEq(
         acn1.base.bitStream.buf,
         acn2.base.bitStream.buf,
         0,
         BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 64
      ))

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, i1) = acn1.dec_Int_PositiveInteger_ConstSize_little_endian_64_pure()
      val (acn2Res, i2) = acn2Reset.dec_Int_PositiveInteger_ConstSize_little_endian_64_pure()

      {
         arrayBitRangesEqSlicedLemma(acn1.base.bitStream.buf, acn2.base.bitStream.buf, 0, BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 64, 0, BitStream.bitIndex(acn1.base.bitStream.buf.length, acn1.base.bitStream.currentByte, acn1.base.bitStream.currentBit) + 32)
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
         BitStream.bitIndex(acn1Res.base.bitStream.buf.length, acn1Res.base.bitStream.currentByte, acn1Res.base.bitStream.currentBit) == BitStream.bitIndex(acn2Res.base.bitStream.buf.length, acn2Res.base.bitStream.currentByte, acn2Res.base.bitStream.currentBit) && i1 == i2
      }
   }

   // TODO: Incomplete specs
   // @extern
   @ghost @pure @opaque @inlineOnce
   def dec_IA5String_CharIndex_External_Field_DeterminantVec_prefixLemma(acn1: ACN, acn2: ACN, max: Long, extSizeDeterminantFld: Long): Unit = {
      require(max < Int.MaxValue)
      require(extSizeDeterminantFld >= 0)
      require(max >= 0)
      require(acn1.base.bufLength() == acn2.base.bufLength())

      val acn2Reset = acn2.resetAt(acn1)
      val (acn1Res, v1) = acn1.dec_IA5String_CharIndex_External_Field_DeterminantVec_pure(max, extSizeDeterminantFld)
      val (acn2Res, v2) = acn2Reset.dec_IA5String_CharIndex_External_Field_DeterminantVec_pure(max, extSizeDeterminantFld)

      {
         ()
      }.ensuring { _ =>
         acn1Res.base.bitStream.bitIndex == acn2Res.base.bitStream.bitIndex && v1 == v2
      }
   }
}
case class ACN(base: Codec) {
   import BitStream.*
   import ACN.*
   import base.*
   // export base.{isPrefixOf => _, withMovedBitIndex => _, withMovedByteIndex => _, *}

   /*ACN Integer functions*/

   def enc_Int_PositiveInteger_ConstSize(intVal: Int, encodedSizeInBits: Int): Unit = {
      val nBits: Int = GetBitCountUnsigned(intVal.toLong.toRawULong)
      require(nBits <= encodedSizeInBits && encodedSizeInBits <= 64)
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, encodedSizeInBits))
      enc_Int_PositiveInteger_ConstSize(intVal.toLong.toRawULong, encodedSizeInBits)
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.base.bitStream.buf.length == w3.base.bitStream.buf.length && BitStream.bitIndex(w3.base.bitStream.buf.length, w3.base.bitStream.currentByte, w3.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + encodedSizeInBits
      && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, encodedSizeInBits)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_pure(encodedSizeInBits)
         iGot.toRaw.toInt == intVal && r3Got == r3
      }
   }

   // @opaque @inlineOnce
   def enc_Int_PositiveInteger_ConstSize(intVal: ULong, encodedSizeInBits: Int): Unit = {
      require(encodedSizeInBits >= 0 && encodedSizeInBits <= 64)
      /* Get number of bits*/
      val nBits: Int = GetBitCountUnsigned(intVal)
      require(nBits <= encodedSizeInBits && encodedSizeInBits <= 64)
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, encodedSizeInBits))

      @ghost val this1 = snapshot(this)
      if (encodedSizeInBits != 0) {
         /* put required zeros*/
         val diff = encodedSizeInBits - nBits
         appendNZeroBits(diff)
         @ghost val this2 = snapshot(this)
         ghostExpr {
            BitStream.validateOffsetBitsDifferenceLemma(this1.base.bitStream, this.base.bitStream, encodedSizeInBits, diff)
         }
         /*Encode number */
         encodeUnsignedInteger(intVal)
         ghostExpr {
            assert(BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) == BitStream.bitIndex(this2.base.bitStream.buf.length, this2.base.bitStream.currentByte, this2.base.bitStream.currentBit) + nBits)
            assert(BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) == BitStream.bitIndex(this1.base.bitStream.buf.length, this1.base.bitStream.currentByte, this1.base.bitStream.currentBit) + encodedSizeInBits)
            lemmaIsPrefixTransitive(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
            val this2Reset = this2.resetAt(this1)
            val (r1_1, r3_1) = ACN.reader(this1, this)
            validateOffsetBitsContentIrrelevancyLemma(this1.base.bitStream, this.base.bitStream.buf, encodedSizeInBits)
            val (r3Got, iGot) = r1_1.dec_Int_PositiveInteger_ConstSize_pure(encodedSizeInBits)

            check(this1.base.bufLength() == this.base.bufLength())
            check(BitStream.bitIndex(this.base.bitStream.buf.length, this.base.bitStream.currentByte, this.base.bitStream.currentBit) == BitStream.bitIndex(this1.base.bitStream.buf.length, this1.base.bitStream.currentByte, this1.base.bitStream.currentBit)  + encodedSizeInBits )

            if (encodedSizeInBits != nBits) {
               checkBitsLoopPrefixLemma2(this2Reset.base.bitStream, this.base.bitStream, diff, false, 0)

               val (r2_2, r3_2) = ACN.reader(this2, this)
               assert(r3_1 == r3_2)
               validateOffsetImpliesMoveBits(r1_1.base.bitStream, diff)
               assert(r2_2 == r1_1.withMovedBitIndex(diff))
               // TODO: Exported symbol not working
               // val (r2Got_2, vGot_2) = r2_2.readNLSBBitsMSBFirstLoopPure(nBits, 0, 0L)
               val (r2Got_2, vGot_2) = r2_2.base.bitStream.readNLSBBitsMSBFirstLoopPure(nBits, 0, 0L)
               assert(vGot_2 == intVal.toRaw)

               val (r3Got_3, vGot_3) = r1_1.base.bitStream.readNLSBBitsMSBFirstLoopPure(encodedSizeInBits, 0, 0L)
               assert(iGot.toRaw == vGot_3)
               assert(r3Got.base.bitStream == r3Got_3)
               checkBitsLoopAndReadNLSB(r1_1.base.bitStream, diff, false)
               readNLSBBitsMSBFirstLeadingZerosLemma(r1_1.base.bitStream, encodedSizeInBits, diff)
               check(iGot == intVal)
               check(r3Got == r3_1)
            } else {
               check(iGot == intVal)
               check(r3Got == r3_1)
            }
         }
      } else {
         ghostExpr {
            lemmaIsPrefixRefl(bitStream)
         }
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.base.bitStream.buf.length == w3.base.bitStream.buf.length && BitStream.bitIndex(w3.base.bitStream.buf.length, w3.base.bitStream.currentByte, w3.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + encodedSizeInBits
      && w1.isPrefixOf(w3) && {
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
   }.ensuring(res => res.base.bitStream.buf == this.base.bitStream.buf && res.base.bitStream.currentByte == other.base.bitStream.currentByte && res.base.bitStream.currentBit == other.base.bitStream.currentBit)

   @ghost @pure @inline
   def withMovedByteIndex(diffInBytes: Int): ACN = {
      require(moveByteIndexPrecond(bitStream, diffInBytes))
      ACN(Codec(bitStream.withMovedByteIndex(diffInBytes)))
   }

   @ghost @pure @inline
   def withMovedBitIndex(diffInBits: Long): ACN = {
      require(moveBitIndexPrecond(bitStream, diffInBits))
      ACN(Codec(bitStream.withMovedBitIndex(diffInBits)))
   }.ensuring(res =>
      this.base.bitStream.bitIndex + diffInBits == res.base.bitStream.bitIndex
      && this.base.bitStream.buf.length == res.base.bitStream.buf.length
   )

   @pure @inline
   def isPrefixOf(acn2: ACN): Boolean = bitStream.isPrefixOf(acn2.base.bitStream)


   @inline @pure @ghost
   def withAlignedToByte(): ACN = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length.toLong, base.bitStream.currentByte.toLong, base.bitStream.currentBit.toLong,
         (NO_OF_BITS_IN_BYTE - base.bitStream.currentBit) & (NO_OF_BITS_IN_BYTE - 1)
      ))
      ACN(base.withAlignedToByte())
   }

   @inline @pure @ghost
   def withAlignedToShort(): ACN = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length.toLong, base.bitStream.currentByte.toLong, base.bitStream.currentBit.toLong,
         (NO_OF_BITS_IN_SHORT -                                                                                               // max alignment (16) -
            (NO_OF_BITS_IN_BYTE * (base.bitStream.currentByte & (NO_OF_BYTES_IN_JVM_SHORT - 1)) + base.bitStream.currentBit)  // current pos
            ) & (NO_OF_BITS_IN_SHORT - 1))                                                                                    // edge case (0,0) -> 0
      )
      ACN(base.withAlignedToShort())
   }

   @inline @pure @ghost
   def withAlignedToInt(): ACN = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length.toLong, base.bitStream.currentByte.toLong, base.bitStream.currentBit.toLong,
         (NO_OF_BITS_IN_INT -                                                                                              // max alignment (32) -
            (NO_OF_BITS_IN_BYTE * (base.bitStream.currentByte & (NO_OF_BYTES_IN_JVM_INT - 1)) + base.bitStream.currentBit) // current pos
            ) & (NO_OF_BITS_IN_INT - 1))                                                                                   // edge case (0,0) -> 0
      )
      ACN(base.withAlignedToInt())
   }

   // @opaque @inlineOnce
   def enc_Int_PositiveInteger_ConstSize_8(intVal: ULong): Unit = {
      require(BitStream.validate_offset_byte(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit))
      require(intVal <= 255)
      appendByte(wrappingExpr { intVal.toUByte })
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      w1.base.bufLength() == w2.base.bufLength() && BitStream.bitIndex(w2.base.bitStream.buf.length, w2.base.bitStream.currentByte, w2.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + 8
      && w1.isPrefixOf(w2) && {
         val (r1, r2) = ACN.reader(w1, w2)
         val (r2Got, vGot) = r1.dec_Int_PositiveInteger_ConstSize_8_pure()
         vGot == intVal && r2Got == r2
      }
   }

   def enc_Int_PositiveInteger_ConstSize_big_endian_16(uintVal: ULong): Unit = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 16))
      require(uintVal <= 65535)
      val intVal = uintVal.toRaw
      assert((intVal >> 16) == 0L)
      @ghost val this1 = snapshot(this)
      appendByte(wrappingExpr { (intVal >> 8).toByte.toRawUByte })
      @ghost val this2 = snapshot(this)
      appendByte(wrappingExpr { intVal.toByte.toRawUByte })
      ghostExpr {
         // For isPrefix
         lemmaIsPrefixTransitive(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         // Reading back the first byte gives the same result whether we are reading from this2 or the end result this
         val this2Reset = this2.base.bitStream.resetAt(this1.base.bitStream)
         readBytePrefixLemma(this2Reset, this.base.bitStream)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.base.bufLength() == w3.base.bufLength() && BitStream.bitIndex(w3.base.bitStream.buf.length, w3.base.bitStream.currentByte, w3.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + 16
      && w1.isPrefixOf(w3)
      && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 16)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_big_endian_16_pure()
         iGot == uintVal && r3Got == r3
      }
   }

   @inlineOnce @opaque
   def enc_Int_PositiveInteger_ConstSize_big_endian_32(uintVal: ULong): Unit = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 32))
      require(uintVal <= 4294967295L)
      val intVal = uintVal.toRaw
      assert((intVal >> 32) == 0L)
      @ghost val this1 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_big_endian_16(wrappingExpr { ((intVal >> 16) & 0xFFFFL).toRawULong })
      @ghost val this2 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_big_endian_16(wrappingExpr { (intVal & 0xFFFFL).toRawULong })
      ghostExpr {
         // For isPrefix
         lemmaIsPrefixTransitive(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         // Reading back the first integer gives the same result whether we are reading from this2 or the end result this
         val this2Reset = this2.resetAt(this1)
         dec_Int_PositiveInteger_ConstSize_big_endian_16_prefixLemma(this2Reset, this)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.base.bufLength() == w3.base.bufLength() && BitStream.bitIndex(w3.base.bitStream.buf.length, w3.base.bitStream.currentByte, w3.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + 32
      && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 32)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_big_endian_32_pure()
         iGot == uintVal && r3Got == r3
      }
   }

   @opaque @inlineOnce
   def enc_Int_PositiveInteger_ConstSize_big_endian_64(uintVal: ULong): Unit = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 64))
      val intVal = uintVal.toRaw
      @ghost val this1 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_big_endian_32(wrappingExpr { ((intVal >> 32) & 0xFFFFFFFFL).toRawULong })
      @ghost val this2 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_big_endian_32(wrappingExpr { (intVal & 0xFFFFFFFFL).toRawULong })
      ghostExpr {
         // For isPrefix
         lemmaIsPrefixTransitive(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         // Reading back the first integer gives the same result whether we are reading from this2 or the end result this
         val this2Reset = this2.resetAt(this1)
         dec_Int_PositiveInteger_ConstSize_big_endian_32_prefixLemma(this2Reset, this)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.base.bufLength() == w3.base.bufLength() && BitStream.bitIndex(w3.base.bitStream.buf.length, w3.base.bitStream.currentByte, w3.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + 64
      && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 64)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_big_endian_64_pure()
         iGot == uintVal && r3Got == r3
      }
   }

   @opaque @inlineOnce
   def enc_Int_PositiveInteger_ConstSize_little_endian_16(uintVal: ULong): Unit = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 16))
      require(uintVal <= 65535)
      val intVal = uintVal.toRaw
      assert((intVal >> 16) == 0L)
      @ghost val this1 = snapshot(this)
      appendByte(wrappingExpr { intVal.toUByte })
      @ghost val this2 = snapshot(this)
      appendByte(wrappingExpr { (intVal >> 8).toUByte })
      ghostExpr {
         // For isPrefix
         lemmaIsPrefixTransitive(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         // Reading back the first byte gives the same result whether we are reading from this2 or the end result this
         val this2Reset = this2.resetAt(this1)
         readBytePrefixLemma(this2Reset.base.bitStream, this.base.bitStream)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.base.bufLength() == w3.base.bufLength() && BitStream.bitIndex(w3.base.bitStream.buf.length, w3.base.bitStream.currentByte, w3.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + 16
      && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 16)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_little_endian_16_pure()
         iGot == uintVal && r3Got == r3
      }
   }

   @opaque @inlineOnce
   def enc_Int_PositiveInteger_ConstSize_little_endian_32(uintVal: ULong): Unit = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 32))
      require(uintVal <= 4294967295L)
      val intVal = uintVal.toRaw
      assert((intVal >> 32) == 0L)
      @ghost val this1 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_little_endian_16(wrappingExpr { (intVal & 0xFFFFL).toRawULong })
      @ghost val this2 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_little_endian_16(wrappingExpr { ((intVal >> 16) & 0xFFFFL).toRawULong })
      ghostExpr {
         // For isPrefix
         lemmaIsPrefixTransitive(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         // Reading back the first integer gives the same result whether we are reading from this2 or the end result this
         val this2Reset = this2.resetAt(this1)
         dec_Int_PositiveInteger_ConstSize_little_endian_16_prefixLemma(this2Reset, this)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.base.bufLength() == w3.base.bufLength() && BitStream.bitIndex(w3.base.bitStream.buf.length, w3.base.bitStream.currentByte, w3.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + 32
      && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 32)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_little_endian_32_pure()
         iGot == uintVal && r3Got == r3
      }
   }

   @opaque @inlineOnce
   def enc_Int_PositiveInteger_ConstSize_little_endian_64(uintVal: ULong): Unit = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 64))
      val intVal = uintVal.toRaw
      @ghost val this1 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_little_endian_32(wrappingExpr { (intVal & 0xFFFFFFFFL).toRawULong })
      @ghost val this2 = snapshot(this)
      enc_Int_PositiveInteger_ConstSize_little_endian_32(wrappingExpr { ((intVal >> 32) & 0xFFFFFFFFL).toRawULong })
      ghostExpr {
         // For isPrefix
         lemmaIsPrefixTransitive(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         // Reading back the first integer gives the same result whether we are reading from this2 or the end result this
         val this2Reset = this2.resetAt(this1)
         dec_Int_PositiveInteger_ConstSize_little_endian_32_prefixLemma(this2Reset, this)
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.base.bufLength() == w3.base.bufLength() && BitStream.bitIndex(w3.base.bitStream.buf.length, w3.base.bitStream.currentByte, w3.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + 64
      && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 64)
         val (r3Got, iGot) = r1.dec_Int_PositiveInteger_ConstSize_little_endian_64_pure()
         iGot == uintVal && r3Got == r3
      }
   }

   def dec_Int_PositiveInteger_ConstSize(encodedSizeInBits: Int): ULong = {
      require(encodedSizeInBits >= 0 && encodedSizeInBits <= NO_OF_BITS_IN_LONG)
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, encodedSizeInBits))
      decodeUnsignedInteger(encodedSizeInBits)
   }

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_pure(encodedSizeInBits: Int): (ACN, ULong) = {
      require(encodedSizeInBits >= 0 && encodedSizeInBits <= NO_OF_BITS_IN_LONG)
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, encodedSizeInBits))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize(encodedSizeInBits)
      (cpy, l)
   }

   def dec_Int_PositiveInteger_ConstSize_8(): ULong = {
      require(BitStream.validate_offset_byte(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit))
      ULong.fromRaw(readByte().toRaw & 0xFF)
   }.ensuring(_ => bitStream.buf == old(this).base.bitStream.buf && BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) == BitStream.bitIndex(old(this).base.bitStream.buf.length, old(this).base.bitStream.currentByte, old(this).base.bitStream.currentBit) + 8)

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_8_pure(): (ACN, ULong) = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 8))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize_8()
      (cpy, l)
   }


   def dec_Int_PositiveInteger_ConstSize_big_endian_16(): ULong = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 16))
      val b1 = readByte().toRaw
      val b2 = readByte().toRaw
      ULong.fromRaw((((b1.toLong << 8) & 0xFF00L) | (b2.toLong & 0xFFL)) & 0xFFFFL)
   }.ensuring(_ => bitStream.buf == old(this).base.bitStream.buf && BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) == BitStream.bitIndex(old(this).base.bitStream.buf.length, old(this).base.bitStream.currentByte, old(this).base.bitStream.currentBit) + 16)

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_big_endian_16_pure(): (ACN, ULong) = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 16))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize_big_endian_16()
      (cpy, l)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_32(): ULong = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 32))
      val i1 = dec_Int_PositiveInteger_ConstSize_big_endian_16().toRaw
      val i2 = dec_Int_PositiveInteger_ConstSize_big_endian_16().toRaw
      ULong.fromRaw((((i1.toLong << 16) & 0xFFFF0000L) | (i2.toLong & 0xFFFFL)) & 0xFFFFFFFFL)
   }.ensuring(_ => bitStream.buf == old(this).base.bitStream.buf && BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) == BitStream.bitIndex(old(this).base.bitStream.buf.length, old(this).base.bitStream.currentByte, old(this).base.bitStream.currentBit) + 32)

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_big_endian_32_pure(): (ACN, ULong) = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 32))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize_big_endian_32()
      (cpy, l)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_64(): ULong = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 64))
      val i1 = dec_Int_PositiveInteger_ConstSize_big_endian_32().toRaw
      val i2 = dec_Int_PositiveInteger_ConstSize_big_endian_32().toRaw
      ULong.fromRaw(((i1.toLong << 32) & 0xFFFFFFFF00000000L) | (i2.toLong & 0xFFFFFFFFL))
   }.ensuring(_ => bitStream.buf == old(this).base.bitStream.buf && BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) == BitStream.bitIndex(old(this).base.bitStream.buf.length, old(this).base.bitStream.currentByte, old(this).base.bitStream.currentBit) + 64)

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_big_endian_64_pure(): (ACN, ULong) = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 64))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize_big_endian_64()
      (cpy, l)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_16(): ULong = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 16))
      val b1 = readByte().toRaw
      val b2 = readByte().toRaw
      ULong.fromRaw((((b2.toLong << 8) & 0xFF00L) | (b1.toLong & 0xFFL)) & 0xFFFFL)
   }.ensuring(_ => bitStream.buf == old(this).base.bitStream.buf && BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) == BitStream.bitIndex(old(this).base.bitStream.buf.length, old(this).base.bitStream.currentByte, old(this).base.bitStream.currentBit) + 16)

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_little_endian_16_pure(): (ACN, ULong) = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 16))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize_little_endian_16()
      (cpy, l)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_32(): ULong = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 32))
      val i1 = dec_Int_PositiveInteger_ConstSize_little_endian_16().toRaw
      val i2 = dec_Int_PositiveInteger_ConstSize_little_endian_16().toRaw
      ULong.fromRaw((((i2.toLong << 16) & 0xFFFF0000L) | (i1.toLong & 0xFFFFL)) & 0xFFFFFFFFL)
   }.ensuring(_ => bitStream.buf == old(this).base.bitStream.buf && BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) == BitStream.bitIndex(old(this).base.bitStream.buf.length, old(this).base.bitStream.currentByte, old(this).base.bitStream.currentBit) + 32)

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_little_endian_32_pure(): (ACN, ULong) = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 32))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize_little_endian_32()
      (cpy, l)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_64(): ULong = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 64))
      val i1 = dec_Int_PositiveInteger_ConstSize_little_endian_32().toRaw
      val i2 = dec_Int_PositiveInteger_ConstSize_little_endian_32().toRaw
      ULong.fromRaw(((i2.toLong << 32) & 0xFFFFFFFF00000000L) | (i1.toLong & 0xFFFFFFFFL))
   }.ensuring(_ => bitStream.buf == old(this).base.bitStream.buf && BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) == BitStream.bitIndex(old(this).base.bitStream.buf.length, old(this).base.bitStream.currentByte, old(this).base.bitStream.currentBit) + 64)

   @ghost @pure
   def dec_Int_PositiveInteger_ConstSize_little_endian_64_pure(): (ACN, ULong) = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 64))
      val cpy = snapshot(this)
      val l = cpy.dec_Int_PositiveInteger_ConstSize_little_endian_64()
      (cpy, l)
   }

   def encode_UnsignedInteger(v: ULong, nBytes: Byte): Unit = {
      val MAX_BYTE_MASK = 0xFF00000000000000L
      assert(nBytes <= 8)

      var vv = v.toRaw << (NO_OF_BYTES_IN_JVM_LONG * 8 - nBytes * 8)

      var i: Int = 0
      (while (i < nBytes) {
         decreases(nBytes - i)
         val byteToEncode = ((vv & MAX_BYTE_MASK) >>> ((NO_OF_BYTES_IN_JVM_LONG - 1) * 8)).toByte
         appendByte(byteToEncode.toRawUByte)
         vv <<= 8
         i += 1
      })
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
   // @opaque @inlineOnce
   def enc_Int_TwosComplement_ConstSize(v: Long, formatBitLength: Int): Unit = {
      val nBits = GetBitCountSigned(v)
      require(nBits <= formatBitLength && formatBitLength <= NO_OF_BITS_IN_LONG)
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, formatBitLength))

      val addedBits = formatBitLength - nBits
      @ghost val this1 = snapshot(this)
      appendNBits(addedBits, v < 0)
      ghostExpr {
         validateOffsetBitsDifferenceLemma(this1.base.bitStream, this.base.bitStream, formatBitLength, addedBits)
      }
      @ghost val this2 = snapshot(this)
      appendLSBBitsMSBFirst(v & onesLSBLong(nBits), nBits)
      ghostExpr {
         assert(BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) == BitStream.bitIndex(this2.base.bitStream.buf.length, this2.base.bitStream.currentByte, this2.base.bitStream.currentBit) + nBits)
         assert(BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) == BitStream.bitIndex(this1.base.bitStream.buf.length, this1.base.bitStream.currentByte, this1.base.bitStream.currentBit) + formatBitLength)
         lemmaIsPrefixTransitive(this1.base.bitStream, this2.base.bitStream, this.base.bitStream)
         val this2Reset = this2.resetAt(this1)
         val (r1_1, r3_1) = ACN.reader(this1, this)
         validateOffsetBitsContentIrrelevancyLemma(this1.base.bitStream, this.base.bitStream.buf, formatBitLength)
         val (r3Got, vGot) = r1_1.dec_Int_TwosComplement_ConstSize_pure(formatBitLength)

         if (formatBitLength != nBits) {
            checkBitsLoopPrefixLemma2(this2Reset.base.bitStream, this.base.bitStream, addedBits, v < 0, 0)

            val (r2_2, r3_2) = ACN.reader(this2, this)
            assert(r3_1 == r3_2)
            validateOffsetImpliesMoveBits(r1_1.base.bitStream, addedBits)
            assert(r2_2 == r1_1.withMovedBitIndex(addedBits))
            val (r2Got_2, vGot_2) = r2_2.base.bitStream.readNLSBBitsMSBFirstLoopPure(nBits, 0, 0L)
            assert(vGot_2 == (v & onesLSBLong(nBits)))

            val (r3Got_3, vGot_3) = r1_1.base.bitStream.readNLSBBitsMSBFirstLoopPure(formatBitLength, 0, 0L)

            checkBitsLoopAndReadNLSB(r1_1.base.bitStream, addedBits, v < 0)
            readNLSBBitsMSBFirstLeadingBitsLemma(r1_1.base.bitStream, v < 0, formatBitLength, addedBits)
            assert(vGot == (bitMSBLong(v < 0, NO_OF_BITS_IN_LONG - formatBitLength) | vGot_3))
            assert(r3Got.base.bitStream == r3Got_3)
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
      w1.base.bufLength() == w3.base.bufLength() && BitStream.bitIndex(w3.base.bitStream.buf.length, w3.base.bitStream.currentByte, w3.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + formatBitLength
      && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, formatBitLength)
         val (r3Got, vGot) = r1.dec_Int_TwosComplement_ConstSize_pure(formatBitLength)
         vGot == v && r3Got == r3
      }
   }

   def enc_Int_TwosComplement_ConstSize_8(intVal: Long): Unit = {
      require(BitStream.validate_offset_byte(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit))
      require(-128L <= intVal && intVal <= 127L)
      enc_Int_PositiveInteger_ConstSize_8(ULong.fromRaw(intVal & 0xFFL))
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.base.bufLength() == w3.base.bufLength() && BitStream.bitIndex(w3.base.bitStream.buf.length, w3.base.bitStream.currentByte, w3.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + 8
      && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 8)
         val (r3Got, iGot) = r1.dec_Int_TwosComplement_ConstSize_8_pure()
         iGot == intVal && r3Got == r3
      }
   }

   def enc_Int_TwosComplement_ConstSize_big_endian_16(intVal: Long): Unit = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 16))
      require(-32768L <= intVal && intVal <= 32767L)
      enc_Int_PositiveInteger_ConstSize_big_endian_16(ULong.fromRaw(intVal & 0xFFFFL))
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.base.bufLength() == w3.base.bufLength() && BitStream.bitIndex(w3.base.bitStream.buf.length, w3.base.bitStream.currentByte, w3.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + 16
      && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 16)
         val (r3Got, iGot) = r1.dec_Int_TwosComplement_ConstSize_big_endian_16_pure()
         iGot == intVal && r3Got == r3
      }
   }

   def enc_Int_TwosComplement_ConstSize_big_endian_32(intVal: Long): Unit = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 32))
      require(-2147483648L <= intVal && intVal <= 2147483647L)
      enc_Int_PositiveInteger_ConstSize_big_endian_32(ULong.fromRaw(intVal & 0xFFFFFFFFL))
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.base.bufLength() == w3.base.bufLength() && BitStream.bitIndex(w3.base.bitStream.buf.length, w3.base.bitStream.currentByte, w3.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + 32
      && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 32)
         val (r3Got, iGot) = r1.dec_Int_TwosComplement_ConstSize_big_endian_32_pure()
         iGot == intVal && r3Got == r3
      }
   }

   def enc_Int_TwosComplement_ConstSize_big_endian_64(intVal: Long): Unit = {
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 64))
      enc_Int_PositiveInteger_ConstSize_big_endian_64(ULong.fromRaw(intVal))
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.base.bufLength() == w3.base.bufLength() && BitStream.bitIndex(w3.base.bitStream.buf.length, w3.base.bitStream.currentByte, w3.base.bitStream.currentBit) == BitStream.bitIndex(w1.base.bitStream.buf.length, w1.base.bitStream.currentByte, w1.base.bitStream.currentBit) + 64
      && w1.isPrefixOf(w3) && {
         val (r1, r3) = ACN.reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1.base.bitStream, w3.base.bitStream.buf, 64)
         val (r3Got, iGot) = r1.dec_Int_TwosComplement_ConstSize_big_endian_64_pure()
         iGot == intVal && r3Got == r3
      }
   }

   def enc_Int_TwosComplement_ConstSize_little_endian_16(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_little_endian_16(ULong.fromRaw(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_little_endian_32(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_little_endian_32(ULong.fromRaw(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_little_endian_64(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_little_endian_64(ULong.fromRaw(intVal))
   }

   def dec_Int_TwosComplement_ConstSize(encodedSizeInBits: Int): Long = {
      require(encodedSizeInBits >= 0 && encodedSizeInBits <= NO_OF_BITS_IN_LONG)
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, encodedSizeInBits))

      val res = readNLSBBitsMSBFirst(encodedSizeInBits)
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
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, encodedSizeInBits))

      val cpy = snapshot(this)
      val l = cpy.dec_Int_TwosComplement_ConstSize(encodedSizeInBits)
      (cpy, l)
   }

   @ghost @pure
   def dec_Int_TwosComplement_ConstSize_8_pure(): (ACN, Long) = {

      val cpy = snapshot(this)
      val l = cpy.dec_Int_TwosComplement_ConstSize_8()
      (cpy, l)
   }

   def dec_Int_TwosComplement_ConstSize_8(): Long = {
      if(!BitStream.validate_offset_byte(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) ) then
         0L
      else
         uint2int(dec_Int_PositiveInteger_ConstSize_8(), 1)
   }

   @ghost @pure
   def dec_Int_TwosComplement_ConstSize_big_endian_16_pure(): (ACN, Long) = {

      val cpy = snapshot(this)
      val l = cpy.dec_Int_TwosComplement_ConstSize_big_endian_16()
      (cpy, l)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_16(): Long = {
      if(!BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 16) ) then
         0L
      else
         uint2int(dec_Int_PositiveInteger_ConstSize_big_endian_16(), NO_OF_BYTES_IN_JVM_SHORT)
   }

   @ghost @pure
   def dec_Int_TwosComplement_ConstSize_big_endian_32_pure(): (ACN, Long) = {

      val cpy = snapshot(this)
      val l = cpy.dec_Int_TwosComplement_ConstSize_big_endian_32()
      (cpy, l)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_32(): Long = {
      if(!BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 32)) then
         0L
      else
         uint2int(dec_Int_PositiveInteger_ConstSize_big_endian_32(), NO_OF_BYTES_IN_JVM_INT)
   }

   @ghost @pure
   def dec_Int_TwosComplement_ConstSize_big_endian_64_pure(): (ACN, Long) = {

      val cpy = snapshot(this)
      val l = cpy.dec_Int_TwosComplement_ConstSize_big_endian_64()
      (cpy, l)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_64(): Long = {
      if(!BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 64)) then
         0L
      else
         uint2int(dec_Int_PositiveInteger_ConstSize_big_endian_64(), NO_OF_BYTES_IN_JVM_LONG)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_16(): Long = {
      if(!BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 16) ) then
         0L
      else
         uint2int(dec_Int_PositiveInteger_ConstSize_little_endian_16(), NO_OF_BYTES_IN_JVM_SHORT)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_32(): Long = {
      if(!BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 32)) then
         0L
      else
         uint2int(dec_Int_PositiveInteger_ConstSize_little_endian_32(), NO_OF_BYTES_IN_JVM_INT)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_64(): Long = {
      if(!BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, 64)) then
         0L
      else
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
         decreases(100 - i)
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
      (while (j < null_characters_size){
         decreases(null_characters_size - j)
         tmp(j) = readByte().toRaw
         j += 1
      }).invariant(true) // TODO invariant

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
   @opaque @inlineOnce
   def BitStream_ReadBitPattern(patternToRead: Array[UByte], nBitsToRead: Int): Boolean = {
      require(nBitsToRead < Int.MaxValue - 8)
      require(nBitsToRead >= 0)
      require(patternToRead.length >=  nBitsToRead / 8 + (if (nBitsToRead % 8 == 0) then 0 else 1))
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, nBitsToRead))
      val nBytesToRead: Int = nBitsToRead / 8
      val nRemainingBitsToRead: Int = nBitsToRead % 8
      val neededBytes = nBytesToRead + (if (nRemainingBitsToRead == 0) then 0 else 1)

      @ghost val oldThis = snapshot(this)
      var pBoolValue: Boolean = true
      var i: Int = 0

      assert(i >= 0)
      assert(i <= nBytesToRead)
      assert(nBitsToRead < Int.MaxValue)
      assert(neededBytes <= patternToRead.length)
      assert(neededBytes == nBytesToRead || neededBytes == nBytesToRead + 1)
      assert(nBytesToRead <= Int.MaxValue / 8)
      assert(neededBytes <= Int.MaxValue / 8)

      (while i < nBytesToRead do
         decreases(nBytesToRead - i)
         @ghost val oldThisLoop = snapshot(this)
         @ghost val oldBufLen = base.bitStream.buf.length
         @ghost val oldCurrentByte = base.bitStream.currentByte
         @ghost val oldCurrentBit = base.bitStream.currentBit
         @ghost val oldBitIndex = BitStream.bitIndex(oldBufLen, oldCurrentByte, oldCurrentBit)
         val read = {
            val remaining = BitStream.remainingBits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit)
            if (remaining < 8) readPartialByte(remaining.toInt)
            else readByte()
         }
         if read != patternToRead(i) then
            pBoolValue = false

         ghostExpr {
            assert(BitStream.bitIndex(oldBufLen, oldCurrentByte, oldCurrentBit) + 8 >= BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit))
            @ghost val bitIndex = BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit)
            assert(bitIndex == oldBitIndex + 8)
            BitStream.validateOffsetBitsIneqLemma(oldThisLoop.base.bitStream, base.bitStream, nBitsToRead - i * 8L, 8L)
         }
         i += 1
      ).opaque.inline.invariant(
         i >= 0 &&&
         i <= nBytesToRead &&&
         nBitsToRead < Int.MaxValue &&&
         neededBytes <= patternToRead.length &&&
         neededBytes >= nBytesToRead &&&
         neededBytes <= Int.MaxValue / 8 &&&
         nBytesToRead <= Int.MaxValue / 8 &&&
         base.bitStream.buf == oldThis.base.bitStream.buf && base.bitStream.currentByte >= 0 && base.bitStream.currentBit >= 0 &&&
         BitStream.invariant(base.bitStream) &&&
         BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) == BitStream.bitIndex(oldThis.base.bitStream.buf.length, oldThis.base.bitStream.currentByte, oldThis.base.bitStream.currentBit) + i * 8L &&&
         BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, nBitsToRead - i * 8L)
      )


      if nRemainingBitsToRead > 0 then
         if readPartialByte(nRemainingBitsToRead.toByte).toRaw != ((patternToRead(nBytesToRead).toRaw & 0xFF) >>> (8 - nRemainingBitsToRead)) then
            pBoolValue = false
         assert(nBytesToRead.toLong * 8L + nRemainingBitsToRead.toLong == nBitsToRead)
         ghostExpr { check(BitStream.bitIndex(this.base.bitStream.buf.length, this.base.bitStream.currentByte, this.base.bitStream.currentBit) <= BitStream.bitIndex(oldThis.base.bitStream.buf.length, oldThis.base.bitStream.currentByte, oldThis.base.bitStream.currentBit) + nBitsToRead) }

      assert(BitStream.bitIndex(this.base.bitStream.buf.length, this.base.bitStream.currentByte, this.base.bitStream.currentBit) == BitStream.bitIndex(oldThis.base.bitStream.buf.length, oldThis.base.bitStream.currentByte, oldThis.base.bitStream.currentBit) + nBitsToRead)

      pBoolValue
   }.ensuring(_ => buf == old(this).base.bitStream.buf && BitStream.bitIndex(this.base.bitStream.buf.length, this.base.bitStream.currentByte, this.base.bitStream.currentBit) == BitStream.bitIndex(old(this).base.bitStream.buf.length, old(this).base.bitStream.currentByte, old(this).base.bitStream.currentBit) + nBitsToRead)

   @opaque @inlineOnce
   def BitStream_DecodeTrueFalseBoolean(truePattern: Array[UByte], falsePattern: Array[UByte], nBitsToRead: Int): Option[Boolean] = {
      require(nBitsToRead < Int.MaxValue - 8)
      require(nBitsToRead > 0)
      require(truePattern.length == falsePattern.length)
      require(truePattern != falsePattern)
      require(truePattern.length >=  nBitsToRead / 8 + (if (nBitsToRead % 8 == 0) then 0 else 1))
      require(BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, nBitsToRead))
      val nBytesToRead: Int = nBitsToRead / 8
      val nRemainingBitsToRead: Int = nBitsToRead % 8
      val neededBytes = nBytesToRead + (if (nRemainingBitsToRead == 0) then 0 else 1)

      @ghost val oldThis = snapshot(this)
      var result = true
      var i: Int = 0

      assert(i >= 0)
      assert(i <= nBytesToRead)
      assert(nBitsToRead < Int.MaxValue)
      assert(neededBytes <= truePattern.length)
      assert(neededBytes == nBytesToRead || neededBytes == nBytesToRead + 1)
      assert(nBytesToRead <= Int.MaxValue / 8)
      assert(neededBytes <= Int.MaxValue / 8)

      (while i < nBytesToRead do
         decreases(nBytesToRead - i)
         @ghost val oldThisLoop = snapshot(this)
         @ghost val oldBufLen = base.bitStream.buf.length
         @ghost val oldCurrentByte = base.bitStream.currentByte
         @ghost val oldCurrentBit = base.bitStream.currentBit
         @ghost val oldBitIndex = BitStream.bitIndex(oldBufLen, oldCurrentByte, oldCurrentBit)
         val read = {
            val remaining = BitStream.remainingBits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit)
            if (remaining < 8) readPartialByte(remaining.toInt)
            else readByte()
         }
         if (read != truePattern(i) && read != falsePattern(i)) return None()
         if (i == 0) {
            result = (read == truePattern(i))
         } else {
            if (read == truePattern(i) && !result) return None()
         }

         ghostExpr {
            assert(BitStream.bitIndex(oldBufLen, oldCurrentByte, oldCurrentBit) + 8 >= BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit))
            @ghost val bitIndex = BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit)
            assert(bitIndex == oldBitIndex + 8)
            BitStream.validateOffsetBitsIneqLemma(oldThisLoop.base.bitStream, base.bitStream, nBitsToRead - i * 8L, 8L)
         }
         i += 1
      ).opaque.inline.noReturnInvariant(
         i >= 0 &&&
         i <= nBytesToRead &&&
         nBitsToRead < Int.MaxValue &&&
         neededBytes <= truePattern.length &&&
         neededBytes >= nBytesToRead &&&
         neededBytes <= Int.MaxValue / 8 &&&
         nBytesToRead <= Int.MaxValue / 8 &&&
         base.bitStream.buf == oldThis.base.bitStream.buf && base.bitStream.currentByte >= 0 && base.bitStream.currentBit >= 0 &&&
         BitStream.invariant(base.bitStream) &&&
         BitStream.bitIndex(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit) <= BitStream.bitIndex(oldThis.base.bitStream.buf.length, oldThis.base.bitStream.currentByte, oldThis.base.bitStream.currentBit) + i * 8L &&&
         BitStream.validate_offset_bits(base.bitStream.buf.length, base.bitStream.currentByte, base.bitStream.currentBit, nBitsToRead - i * 8L)
      )

      if nRemainingBitsToRead > 0 then
         val read = readPartialByte(nRemainingBitsToRead.toByte).toRaw
         val truePartialPattern = (truePattern(nBytesToRead).toRaw & 0xFF) >>> (8 - nRemainingBitsToRead)
         val falsePartialPattern = (falsePattern(nBytesToRead).toRaw & 0xFF) >>> (8 - nRemainingBitsToRead)
         if (read != truePartialPattern && read != falsePartialPattern) return None()
         if (nBytesToRead == 0) {
            result = (read == truePartialPattern)
         } else {
            if (read == truePartialPattern && !result) return None()
         }
         assert(nBytesToRead.toLong * 8L + nRemainingBitsToRead.toLong == nBitsToRead)
         ghostExpr { check(BitStream.bitIndex(this.base.bitStream.buf.length, this.base.bitStream.currentByte, this.base.bitStream.currentBit) <= BitStream.bitIndex(oldThis.base.bitStream.buf.length, oldThis.base.bitStream.currentByte, oldThis.base.bitStream.currentBit) + nBitsToRead) }

      assert(BitStream.bitIndex(this.base.bitStream.buf.length, this.base.bitStream.currentByte, this.base.bitStream.currentBit) <= BitStream.bitIndex(oldThis.base.bitStream.buf.length, oldThis.base.bitStream.currentByte, oldThis.base.bitStream.currentBit) + nBitsToRead)
      Some(result)
   }.ensuring { (res: Option[Boolean]) =>
      res match {
         case None() => true
         case Some(_) =>
            buf == old(this).base.bitStream.buf && BitStream.bitIndex(this.base.bitStream.buf.length, this.base.bitStream.currentByte, this.base.bitStream.currentBit) <= BitStream.bitIndex(old(this).base.bitStream.buf.length, old(this).base.bitStream.currentByte, old(this).base.bitStream.currentBit) + nBitsToRead
      }
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

   @extern
   def enc_String_Ascii_Null_Terminated_multVec(max: Long, null_character: Array[Byte], null_character_size: Int, strVal: Vector[ASCIIChar]): Unit = {
      enc_String_Ascii_Null_Terminated_mult(max, null_character, null_character_size, strVal.toScala.toArray)
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
         decreases(max -i)
         val charIndex: Int = GetCharIndex(strVal(i), allowedCharSet)
         encodeConstrainedWholeNumber(charIndex, 0, allowedCharSet.length - 1)
         i += 1
   }

   @opaque @inlineOnce
   def enc_String_CharIndex_private(max: Long, allowedCharSet: Array[UByte], strVal: Array[ASCIIChar]): Long = {
      // Does not make sense to have a max variable of type Long, as the index of an array must be an Int
      require(max < Int.MaxValue && max >= 0)
      require(allowedCharSet.length > 0)
      @ghost val oldThis = snapshot(this)
      var i: Int = 0
      // SAM Here I put a dynamic check for the buffer size, because the buffer size depends on when the CHAR_0000 is found, and it does
      // not make sense to require a buffer of size max, and I decided to return -1L if one of the checkes fails
      val nBitsPerElmt = GetBitCountUnsigned(stainless.math.wrapping(allowedCharSet.length - 1).toRawULong)
      (while (i < max) && (i < strVal.length) && (strVal(i).toRaw != CHAR_0000) do
         decreases(max - i)
         if((i >= strVal.length) ) then
            return -1L
         val charIndex: Int = GetCharIndex(strVal(i), allowedCharSet)
         if(!BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nBitsPerElmt)) then
            return -1L
         encodeConstrainedWholeNumber(charIndex, 0, allowedCharSet.length - 1)
         i += 1
      ).invariant(
         i <= max && i >= 0 &&
         max <= Int.MaxValue &&
         i <= strVal.length &&
         base.bitStream.buf.length == oldThis.base.bitStream.buf.length
      )

      i.toLong
   }.ensuring(_ => base.bitStream.buf.length == old(this).base.bitStream.buf.length)

   @opaque @inlineOnce
   def enc_String_CharIndex_External_Field_Determinant(max: Long, allowedCharSet: Array[UByte], strVal: Array[ASCIIChar]): Unit = {
      enc_String_CharIndex_private(max, allowedCharSet, strVal)
      ()
   }.ensuring(_ => base.bitStream.buf.length == old(this).base.bitStream.buf.length)

   @opaque @inlineOnce
   def enc_String_CharIndex_Internal_Field_Determinant(max: Long, allowedCharSet: Array[UByte], min: Long, strVal: Array[ASCIIChar]): Unit = {
      val strLen: Int = strVal.length
      encodeConstrainedWholeNumber(if strLen <= max then strLen else max, min, max)
      enc_String_CharIndex_private(max, allowedCharSet, strVal)
      ()
   }.ensuring(_ => base.bitStream.buf.length == old(this).base.bitStream.buf.length)

   @opaque @inlineOnce
   def enc_IA5String_CharIndex_External_Field_Determinant(max: Long, strVal: Array[ASCIIChar]): Unit = {
      require(max < Int.MaxValue && max >= 0)
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
      ()
   }.ensuring(_ => base.bitStream.buf.length == old(this).base.bitStream.buf.length)

   @extern
   def enc_IA5String_CharIndex_External_Field_DeterminantVec(max: Long, strVal: Vector[ASCIIChar]): Unit = {
      require(max < Int.MaxValue && max >= 0)
      enc_IA5String_CharIndex_External_Field_Determinant(max, strVal.toScala.toArray)
   }.ensuring(_ => base.bitStream.buf.length == old(this).base.bitStream.buf.length)

   @opaque @inlineOnce
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
      ()
   }.ensuring(_ => base.bitStream.buf.length == old(this).base.bitStream.buf.length)

   @extern
   def enc_IA5String_CharIndex_Internal_Field_DeterminantVec(max: Long, min: Long, strVal: Vector[ASCIIChar]): Unit = {
      enc_IA5String_CharIndex_Internal_Field_Determinant(max, min, strVal.toScala.toArray)
   }.ensuring(_ => base.bitStream.buf.length == old(this).base.bitStream.buf.length)


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

   @extern
   def dec_String_Ascii_Null_Terminated_multVec(max: Long, null_character: Array[ASCIIChar], null_character_size: Int): Vector[ASCIIChar] = {
      val res = dec_String_Ascii_Null_Terminated_mult(max, null_character, null_character_size)
      Vector.fromScala(res.toVector)
   }

   @opaque @inlineOnce
   def dec_String_Ascii_External_Field_Determinant(max: Long, extSizeDeterminantFld: Long): Array[ASCIIChar] = {
      dec_String_Ascii_private(max, if extSizeDeterminantFld <= max then extSizeDeterminantFld else max)
   }.ensuring(_ => base.bitStream.buf == old(this).base.bitStream.buf)

   @opaque @inlineOnce
   def dec_String_Ascii_Internal_Field_Determinant(max: Long, min: Long): Array[ASCIIChar] = {
      val nCount = decodeConstrainedWholeNumber(min, max)
      dec_String_Ascii_private(max, if nCount <= max then nCount else max)
   }.ensuring(_ => base.bitStream.buf == old(this).base.bitStream.buf)

   @opaque @inlineOnce
   def dec_String_CharIndex_private(max: Long, charactersToDecode: Long, allowedCharSet: Array[UByte]): Array[ASCIIChar] = {
      require(allowedCharSet.length > 0)
      require(max < Int.MaxValue && max >= 0)
      require(charactersToDecode >= 0 && charactersToDecode <= max)
      @ghost val oldThis = snapshot(this)
      val strVal: Array[ASCIIChar] = Array.fill(max.toInt + 1)(0.toRawUByte)
      var i: Int = 0
      assert(allowedCharSet.length > 0)
      assert(strVal.length > 0)
      assert(strVal.length > max)
      assert(strVal.length > charactersToDecode)
      assert(i < strVal.length)

      val nBitsPerElmt = GetBitCountUnsigned(stainless.math.wrapping(allowedCharSet.length - 1).toRawULong)
      var flag = false
      (while !flag && i < charactersToDecode do
         decreases(charactersToDecode - i)
         if(!BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, nBitsPerElmt))
            // return Array.empty
            flag = true
         else
            strVal(i) = allowedCharSet(decodeConstrainedWholeNumber(0, allowedCharSet.length - 1).toInt)
            i += 1
      ).invariant(
         flag && base.bitStream.buf == oldThis.base.bitStream.buf
         || !flag && (
            i <= charactersToDecode && i >= 0 &&
            charactersToDecode < Int.MaxValue &&
            charactersToDecode <= max &&
            charactersToDecode >= 0 &&
            i < strVal.length &&
            max < strVal.length &&
            strVal.length == max.toInt + 1 &&
            base.bitStream.buf == oldThis.base.bitStream.buf
         )
      )
      if flag then
         Array.empty[ASCIIChar]
      else
         strVal
   }.ensuring(res => base.bitStream.buf == old(this).base.bitStream.buf && (res.length == max.toInt + 1 || res.length == 0))

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


   @opaque @inlineOnce
   def dec_IA5String_CharIndex_External_Field_Determinant(max: Long, extSizeDeterminantFld: Long): Array[ASCIIChar] = {
      require(max < Int.MaxValue)
      require(extSizeDeterminantFld >= 0)
      require(max >= 0) // SAM Check whether this is correct, otherwise transform it into a runtime check
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
   }.ensuring(res => base.bitStream.buf == old(this).base.bitStream.buf && (res.length == max.toInt + 1 || res.length == 0))

   @extern
   def dec_IA5String_CharIndex_External_Field_DeterminantVec(max: Long, extSizeDeterminantFld: Long): Vector[ASCIIChar] = {
      require(max < Int.MaxValue)
      require(extSizeDeterminantFld >= 0)
      require(max >= 0)
      val arr = dec_IA5String_CharIndex_External_Field_Determinant(max, extSizeDeterminantFld)
      Vector.fromScala(arr.toVector)
   }.ensuring(res => base.bitStream.buf == old(this).base.bitStream.buf && res.length == max.toInt + 1)

   @pure @ghost
   def dec_IA5String_CharIndex_External_Field_DeterminantVec_pure(max: Long, extSizeDeterminantFld: Long): (ACN, Vector[ASCIIChar]) = {
      require(max < Int.MaxValue)
      require(extSizeDeterminantFld >= 0)
      require(max >= 0)
      val cpy = snapshot(this)
      val res = cpy.dec_IA5String_CharIndex_External_Field_DeterminantVec(max, extSizeDeterminantFld)
      (cpy, res)
   }

   def dec_IA5String_CharIndex_Internal_Field_Determinant(max: Long, min: Long): Array[ASCIIChar] = {
      require(min <= max)
      require(max < Int.MaxValue)
      require(max >= 0)
      require(min >= 0) // SAM Check whether this is correct, otherwise transform it into a runtime check
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
      if(!BitStream.validate_offset_bits(bitStream.buf.length, bitStream.currentByte, bitStream.currentBit, GetBitCountUnsigned(stainless.math.wrapping(max - min).toRawULong)))
         return Array.empty
      val nCount = decodeConstrainedWholeNumber(min, max)
      val charToDecode = if nCount <= max then nCount else max
      assert(charToDecode >= 0 && charToDecode <= max)
      dec_String_CharIndex_private(max, charToDecode, UByte.fromArrayRaws(allowedCharSet))
   }.ensuring(res => base.bitStream.buf == old(this).base.bitStream.buf && (res.length == max.toInt + 1 || res.length == 0))

   @extern
   def dec_IA5String_CharIndex_Internal_Field_DeterminantVec(max: Long, min: Long): Vector[ASCIIChar] = {
      require(min <= max)
      require(max < Int.MaxValue)
      require(max >= 0)
      require(min >= 0) // SAM Check whether this is correct, otherwise transform it into a runtime check
      val arr = dec_IA5String_CharIndex_Internal_Field_Determinant(max, min)
      Vector.fromScala(arr.toVector)
   }.ensuring(res => base.bitStream.buf == old(this).base.bitStream.buf && res.length == max.toInt + 1)


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
