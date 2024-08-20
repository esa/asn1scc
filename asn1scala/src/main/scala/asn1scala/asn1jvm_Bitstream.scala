package asn1scala

import stainless.*
import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}
import stainless.collection.*
import stainless.annotation.*
import stainless.proof.*
import stainless.math.{wrapping => wrappingExpr, *}
import StaticChecks.*

object BitStream {
   @pure @inline
   final def invariant(bs: BitStream): Boolean = {
      invariant(bs.currentBit, bs.currentByte, bs.buf.length)
   }

   @pure
   final def invariant(currentBit: Int, currentByte: Int, buffLength: Int): Boolean = {
         currentBit >= 0 && currentBit < NO_OF_BITS_IN_BYTE &&
         currentByte >= 0 && ((currentByte < buffLength) || (currentBit == 0 && currentByte == buffLength))
   }

   @pure
   final def remainingBits(bufLength: Long, currentByte: Long, currentBit: Long): Long = {
      require(bufLength <= Int.MaxValue && currentByte <= Int.MaxValue && currentBit <= Int.MaxValue)
      require(bufLength >= 0 && currentByte >= 0 && currentBit >= 0)
      require(invariant(currentBit.toInt, currentByte.toInt, bufLength.toInt))
      (bufLength * NO_OF_BITS_IN_BYTE) - (currentByte * NO_OF_BITS_IN_BYTE + currentBit)
   }

   @pure
   final def validate_offset_bit(bufLength: Long, currentByte: Long, currentBit: Long): Boolean = {
      require(bufLength <= Int.MaxValue && currentByte <= Int.MaxValue && currentBit <= Int.MaxValue)
      require(bufLength >= 0 && currentByte >= 0 && currentBit >= 0)
      require(invariant(currentBit.toInt, currentByte.toInt, bufLength.toInt))
      validate_offset_bits(bufLength, currentByte, currentBit, 1)
   }

   @pure
   final def validate_offset_bits(bufLength: Long, currentByte: Long, currentBit: Long, bits: Long = 0): Boolean = {
      require(bufLength <= Int.MaxValue && currentByte <= Int.MaxValue && currentBit <= Int.MaxValue)
      require(bufLength >= 0 && currentByte >= 0 && currentBit >= 0)
      require(invariant(currentBit.toInt, currentByte.toInt, bufLength.toInt))
      require(bits >= 0)
      BitStream.remainingBits(bufLength, currentByte, currentBit) >= bits
   }

   @pure
   final def validate_offset_byte(bufLength: Long, currentByte: Long, currentBit: Long): Boolean = {
      require(bufLength <= Int.MaxValue && currentByte <= Int.MaxValue && currentBit <= Int.MaxValue)
      require(bufLength >= 0 && currentByte >= 0 && currentBit >= 0)
      require(invariant(currentBit.toInt, currentByte.toInt, bufLength.toInt))
      BitStream.remainingBits(bufLength, currentByte, currentBit) >= NO_OF_BITS_IN_BYTE
   }

   @pure
   final def validate_offset_bytes(bufLength: Long, currentByte: Long, currentBit: Long, bytes: Int): Boolean = {
      require(bufLength <= Int.MaxValue && currentByte <= Int.MaxValue && currentBit <= Int.MaxValue)
      require(bufLength >= 0 && currentByte >= 0 && currentBit >= 0)
      require(invariant(currentBit.toInt, currentByte.toInt, bufLength.toInt))
      require(bytes >= 0)
      bytes <= BitStream.remainingBits(bufLength, currentByte, currentBit) / NO_OF_BITS_IN_BYTE
   }

   @pure
   final def bitIndex(bufLength: Int, currentByte: Int, currentBit: Int): Long = {
      require(invariant(currentBit, currentByte, bufLength))
      currentByte.toLong * NO_OF_BITS_IN_BYTE + currentBit.toLong
   }.ensuring(res =>
         res == bufLength.toLong * NO_OF_BITS_IN_BYTE - BitStream.remainingBits(bufLength.toLong, currentByte.toLong, currentBit.toLong) &&
         0 <= res && res <= bufLength.toLong * 8L
   )

   @ghost @pure
   def readerFrom(w: BitStream, newCurrentBit: Int, newCurrentBytes: Int): BitStream = {
      require(invariant(w.currentBit, w.currentByte, w.buf.length))
      require(invariant(newCurrentBit, newCurrentBytes, w.buf.length))
      BitStream(snapshot(w.buf), newCurrentBytes, newCurrentBit)

   }.ensuring(res => invariant(res.currentBit, res.currentByte, res.buf.length))

   /**
     * Creates two new BitStream instances, with the buffer of w2, and the currentByte and currentBit of w1 and w2 respectively.
     *
     * @param w1
     * @param w2
     * @return
     */
   @ghost @pure
   def reader(w1: BitStream, w2: BitStream): (BitStream, BitStream) = {
      require(w1.isPrefixOf(w2))
      val r1 = BitStream(snapshot(w2.buf), w1.currentByte, w1.currentBit)
      val r2 = BitStream(snapshot(w2.buf), w2.currentByte, w2.currentBit)
      lemmaIsPrefixRefl(w1)
      lemmaIsPrefixRefl(r1)
      lemmaIsPrefixRefl(w2)
      lemmaIsPrefixRefl(r2)

      // Asserts are here as documentation for the proof
      assert((w1.buf.length != 0) ==> arrayBitRangesEq(w1.buf, w2.buf, 0, BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit)))
      if(w1.buf.length != 0) then
         arrayBitRangesEqSymmetric(w1.buf, w2.buf, 0 , BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit ))
      assert((w1.buf.length != 0) ==> arrayBitRangesEq(w2.buf, w1.buf, 0, BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit)))

      assert(r1.isPrefixOf(w1))
      lemmaIsPrefixTransitive(r1, w1, w2)
      lemmaIsPrefixTransitive(r1, w2, r2)
      (r1, r2)
   }.ensuring(res =>
      res._1.isPrefixOf(res._2)
      && res._1.isPrefixOf(w1)
      && res._2.isPrefixOf(w2)
      && res._1 == res._2.withMovedBitIndex(BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) - BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit))
      )

   @ghost @pure @opaque @inlineOnce
   def resetAndThenMovedLemma(b1: BitStream, b2: BitStream, moveInBits: Long): Unit = {
      require(b1.buf.length == b2.buf.length)
      require(moveInBits >= 0)
      require(BitStream.validate_offset_bits(b1.buf.length.toLong, b1.currentByte.toLong, b1.currentBit.toLong, moveInBits))

      val b2Reset = b2.resetAt(b1)

      {
         ()
      }.ensuring(_ => moveBitIndexPrecond(b2Reset, moveInBits))
   }

   @ghost @pure @opaque @inlineOnce
   def eqBufAndBitIndexImpliesEq(b1: BitStream, b2: BitStream): Unit = {
      require(b1.buf == b2.buf)
      require(BitStream.bitIndex(b1.buf.length, b1.currentByte, b1.currentBit ) == BitStream.bitIndex(b2.buf.length, b2.currentByte, b2.currentBit ))
   }.ensuring(_ => b1 == b2)

   @ghost @pure @opaque @inlineOnce
   def validateOffsetBitsIneqLemma(b1: BitStream, b2: BitStream, b1ValidateOffsetBits: Long, advancedAtMostBits: Long): Unit = {
      require(0 <= advancedAtMostBits && advancedAtMostBits <= b1ValidateOffsetBits)
      require(b1.buf.length == b2.buf.length)
      require(BitStream.validate_offset_bits(b1.buf.length.toLong, b1.currentByte.toLong, b1.currentBit.toLong, b1ValidateOffsetBits))
      require(BitStream.bitIndex(b2.buf.length, b2.currentByte, b2.currentBit) <= BitStream.bitIndex(b1.buf.length, b1.currentByte, b1.currentBit) + advancedAtMostBits)

      assert(BitStream.remainingBits(b1.buf.length, b1.currentByte, b1.currentBit) >= b1ValidateOffsetBits)
      assert((b1.buf.length.toLong * NO_OF_BITS_IN_BYTE) - (b1.currentByte.toLong * NO_OF_BITS_IN_BYTE + b1.currentBit) >= b1ValidateOffsetBits)
      assert(b2.currentByte.toLong * NO_OF_BITS_IN_BYTE + b2.currentBit <= b1.currentByte.toLong * NO_OF_BITS_IN_BYTE + b1.currentBit + advancedAtMostBits)
      assert((b1.buf.length.toLong * NO_OF_BITS_IN_BYTE) - (b2.currentByte.toLong * NO_OF_BITS_IN_BYTE + b2.currentBit) >= b1ValidateOffsetBits - advancedAtMostBits)
      assert(BitStream.remainingBits(b2.buf.length, b2.currentByte, b2.currentBit) >= b1ValidateOffsetBits - advancedAtMostBits)
   }.ensuring(_ => BitStream.validate_offset_bits(b2.buf.length.toLong, b2.currentByte.toLong, b2.currentBit.toLong, b1ValidateOffsetBits - advancedAtMostBits))

   @ghost @pure @opaque @inlineOnce
   def validateOffsetBitsWeakeningLemma(b: BitStream, origOffset: Long, newOffset: Long): Unit = {
      require(0 <= newOffset && newOffset <= origOffset)
      require(BitStream.validate_offset_bits(b.buf.length.toLong, b.currentByte.toLong, b.currentBit.toLong, origOffset))
   }.ensuring(_ => BitStream.validate_offset_bits(b.buf.length.toLong, b.currentByte.toLong, b.currentBit.toLong, newOffset))

   @ghost @pure @opaque @inlineOnce
   def validateOffsetBitsDifferenceLemma(b1: BitStream, b2: BitStream, b1ValidateOffsetBits: Long, b1b2Diff: Long): Unit = {
      require(b1.buf.length == b2.buf.length)
      require(0 <= b1ValidateOffsetBits && 0 <= b1b2Diff && b1b2Diff <= b1ValidateOffsetBits)
      require(BitStream.validate_offset_bits(b1.buf.length.toLong, b1.currentByte.toLong, b1.currentBit.toLong, b1ValidateOffsetBits))
      require(BitStream.bitIndex(b1.buf.length, b1.currentByte, b1.currentBit ) + b1b2Diff == BitStream.bitIndex(b2.buf.length, b2.currentByte, b2.currentBit ))

      {
         remainingBitsBitIndexLemma(b1)
         assert(BitStream.remainingBits(b1.buf.length.toLong, b1.currentByte.toLong, b1.currentBit.toLong) == b1.buf.length.toLong * NO_OF_BITS_IN_BYTE - BitStream.bitIndex(b1.buf.length, b1.currentByte, b1.currentBit ))
         assert(BitStream.bitIndex(b1.buf.length, b1.currentByte, b1.currentBit ) <= b1.buf.length.toLong * NO_OF_BITS_IN_BYTE - b1ValidateOffsetBits)

         remainingBitsBitIndexLemma(b2)
         assert(BitStream.remainingBits(b2.buf.length.toLong, b2.currentByte.toLong, b2.currentBit.toLong) == b2.buf.length.toLong * NO_OF_BITS_IN_BYTE - (BitStream.bitIndex(b1.buf.length, b1.currentByte, b1.currentBit ) + b1b2Diff))
         assert(BitStream.remainingBits(b2.buf.length.toLong, b2.currentByte.toLong, b2.currentBit.toLong) >= b1ValidateOffsetBits - b1b2Diff)
      }.ensuring(_ => BitStream.validate_offset_bits(b2.buf.length.toLong, b2.currentByte.toLong, b2.currentBit.toLong, b1ValidateOffsetBits - b1b2Diff))
   }


   @ghost @pure @opaque @inlineOnce
   def remainingBitsBitIndexLemma(b: BitStream): Unit = {
      ()
   }.ensuring(_ => BitStream.remainingBits(b.buf.length.toLong, b.currentByte.toLong, b.currentBit.toLong) == b.buf.length.toLong * NO_OF_BITS_IN_BYTE - BitStream.bitIndex(b.buf.length, b.currentByte, b.currentBit ))

   @ghost @pure @opaque @inlineOnce
   def resetAtEqLemma(b1: BitStream, b2: BitStream, b3: BitStream): Unit = {
      require(b1.buf == b2.buf)
      require(b1.buf.length == b3.buf.length)
      require(b1.bitIndex == b3.bitIndex)
   }.ensuring(_ => b1 == b2.resetAt(b3))

   @ghost @pure @opaque @inlineOnce
   def validateOffsetBytesContentIrrelevancyLemma(b1: BitStream, buf: Array[Byte], bytes: Int): Unit = {
      require(b1.buf.length == buf.length)
      require(bytes >= 0)
      require( BitStream.validate_offset_bytes(b1.buf.length.toLong, b1.currentByte.toLong, b1.currentBit.toLong,bytes))
      val b2 = BitStream(snapshot(buf), b1.currentByte, b1.currentBit)

      {
         ()
      }.ensuring(_ =>  BitStream.validate_offset_bytes(b2.buf.length.toLong, b2.currentByte.toLong, b2.currentBit.toLong,bytes))
   }

   @ghost @pure @opaque @inlineOnce
   def validateOffsetBitsContentIrrelevancyLemma(b1: BitStream, buf: Array[Byte], bits: Long): Unit = {
      require(b1.buf.length == buf.length)
      require(bits >= 0)
      require(BitStream.validate_offset_bits(b1.buf.length.toLong, b1.currentByte.toLong, b1.currentBit.toLong, bits))
      val b2 = BitStream(snapshot(buf), b1.currentByte, b1.currentBit)

      {
         ()
      }.ensuring(_ => BitStream.validate_offset_bits(b2.buf.length.toLong, b2.currentByte.toLong, b2.currentBit.toLong, bits))
   }

   // @ghost @pure @opaque @inlineOnce
   // def validateOffsetBytesFromBitsLemma(b: BitStream, bits: Long, bytes: Int): Unit = {
   //    require(0 <= bytes && bytes <= bits / 8 && 0 <= bits)
   //    require(BitStream.validate_offset_bits(b.buf.length.toLong, b.currentByte.toLong, b.currentBit.toLong, bits))

   //    {
   //       ()
   //    }.ensuring(_ => BitStream.validate_offset_bytes(b.buf.length.toLong, b.currentByte.toLong, b.currentBit.toLong, bytes))
   // }

   @ghost @pure @opaque @inlineOnce
   def validateOffsetBytesFromBitIndexLemma(b1: BitStream, b2: BitStream, bits: Long, bytes: Int): Unit = {
      require(b1.buf.length == b2.buf.length)
      require(0 < bytes && 0 <= bits && bits <= BitStream.bitIndex(b2.buf.length, b2.currentByte, b2.currentBit ))
      require(((bits + 7) / 8).toInt <= bytes)
      require(BitStream.validate_offset_bytes(b1.buf.length.toLong, b1.currentByte.toLong, b1.currentBit.toLong,bytes))
      require(BitStream.bitIndex(b2.buf.length, b2.currentByte, b2.currentBit ) == BitStream.bitIndex(b1.buf.length, b1.currentByte, b1.currentBit ) + bits)

      {
         assert(bytes <= BitStream.remainingBits(b1.buf.length.toLong, b1.currentByte.toLong, b1.currentBit.toLong) / 8)
         assert(bytes <= ((b1.buf.length.toLong * 8) - (b1.currentByte.toLong * 8 + b1.currentBit)) / 8)
         assert(BitStream.bitIndex(b2.buf.length, b2.currentByte, b2.currentBit ) == b2.currentByte.toLong * 8 + b2.currentBit)
         assert(BitStream.bitIndex(b2.buf.length, b2.currentByte, b2.currentBit ) - bits == b1.currentByte.toLong * 8 + b1.currentBit)
         assert(bytes <= ((b2.buf.length.toLong * 8) - (b2.currentByte.toLong * 8 + b2.currentBit - bits)) / 8)
         assert(bytes <= ((b2.buf.length.toLong * 8) - (b2.currentByte.toLong * 8 + b2.currentBit)) / 8 + ((bits + 7) / 8))
         check(BitStream.validate_offset_bytes(b2.buf.length.toLong, b2.currentByte.toLong, b2.currentBit.toLong,bytes - ((bits + 7) / 8).toInt))
      }.ensuring(_ =>  BitStream.validate_offset_bytes(b2.buf.length.toLong, b2.currentByte.toLong, b2.currentBit.toLong,bytes - ((bits + 7) / 8).toInt))
   }

   @ghost @pure @opaque @inlineOnce
   def validateOffsetImpliesMoveBits(b: BitStream, bits: Long): Unit = {
      require(0 <= bits && bits <= b.buf.length.toLong * 8L)
      require(BitStream.validate_offset_bits(b.buf.length.toLong, b.currentByte.toLong, b.currentBit.toLong, bits))

      {
         ()
      }.ensuring(_ => moveBitIndexPrecond(b, bits))
   }

   // For showing invertibility of encoding - not fully integrated yet
   @ghost @pure @opaque @inlineOnce
   def readBytePrefixLemma(bs1: BitStream, bs2: BitStream): Unit = {
      require(bs1.buf.length == bs2.buf.length)
      require(bs1.validate_offset_bits(8))
      require(arrayBitRangesEq(
         bs1.buf,
         bs2.buf,
         0,
         BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) + 8
      ))

      val bs2Reset = BitStream(snapshot(bs2.buf), bs1.currentByte, bs1.currentBit)
      val (bs1Res, b1) = bs1.readBytePure()
      val (bs2Res, b2) = bs2Reset.readBytePure()

      {
         val end = (BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) / 8 + 1).toInt
         arrayRangesEqImpliesEq(bs1.buf, bs2.buf, 0, bs1.currentByte, end)
      }.ensuring { _ =>
         BitStream.bitIndex(bs1Res.buf.length, bs1Res.currentByte, bs1Res.currentBit ) == BitStream.bitIndex(bs2Res.buf.length, bs2Res.currentByte, bs2Res.currentBit ) && b1 == b2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def readByteRangesEq(bs1: BitStream, bs2: BitStream, rangeEqUntil: Long): Unit = {
      require(bs1.buf.length == bs2.buf.length)
      require(8 <= rangeEqUntil && BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) <= rangeEqUntil - 8 && rangeEqUntil <= bs1.buf.length.toLong * 8)
      require(BitStream.validate_offset_byte(bs1.buf.length.toLong, bs1.currentByte.toLong, bs1.currentBit.toLong))
      require(arrayBitRangesEq(
         bs1.buf,
         bs2.buf,
         0,
         rangeEqUntil
      ))

      val bs2Reset = bs2.resetAt(bs1)
      val read1 = bs1.readBytePure()._2
      val read2 = bs2Reset.readBytePure()._2

      {
         val aligned = BitStream.bitIndex(bs1.withAlignedToByte().buf.length, bs1.withAlignedToByte().currentByte, bs1.withAlignedToByte().currentBit )
         arrayBitRangesEqSlicedLemma(bs1.buf, bs2.buf, 0, rangeEqUntil, BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ), aligned)
         arrayBitRangesEqSlicedLemma(bs1.buf, bs2.buf, 0, rangeEqUntil, aligned, BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) + 8)
      }.ensuring { _ =>
         read1 == read2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def readBitPrefixLemma(bs1: BitStream, bs2: BitStream): Unit = {
      require(bs1.buf.length == bs2.buf.length)
      require(bs1.validate_offset_bits(1))
      require(arrayBitRangesEq(
         bs1.buf,
         bs2.buf,
         0,
         BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) + 1
      ))

      val bs2Reset = bs2.resetAt(bs1)
      val (bs1Res, b1) = bs1.readBitPure()
      val (bs2Res, b2) = bs2Reset.readBitPure()

      {
         arrayBitRangesEqImpliesEq(bs1.buf, bs2.buf, 0, BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ), BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) + 1)
      }.ensuring { _ =>
         BitStream.bitIndex(bs1Res.buf.length, bs1Res.currentByte, bs1Res.currentBit ) == BitStream.bitIndex(bs2Res.buf.length, bs2Res.currentByte, bs2Res.currentBit ) && b1 == b2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def lemmaReadNBitsLSBFirstsLoopIsCorrect(bs: BitStream, nBits: Int, i: Int, acc: Long): Unit = {
      require(0 <= i && i < nBits && nBits <= 64)
      require(BitStream.validate_offset_bits(bs.buf.length.toLong, bs.currentByte.toLong, bs.currentBit.toLong, nBits - i))
      require((acc & onesMSBLong(64 - i)) == 0L) //  The 64 - i MSBs must be 0
      require((acc & onesMSBLong(64)) == acc)
      decreases(nBits - i)
      val (bsFinal, vGot1) = bs.readNBitsLSBFirstsLoopPure(nBits, i, acc)
      val readBit = bs.readBitPure()._2
      val bs2 = bs.withMovedBitIndex(1)
      val newAcc = acc | (if readBit then 1L << i else 0)
      val (bs2Final, vGot2) = bs2.readNBitsLSBFirstsLoopPure(nBits, i + 1, newAcc)

      {
         ()
      }.ensuring { _ =>
         vGot1 == vGot2 && bsFinal == bs2Final
      }
   }


   // TODO: "loopPrefixLemma" is a bad name, it's not the same "prefix lemma" as the others!!!
   @ghost @pure @opaque @inlineOnce
   def readNLSBBitsMSBFirstLoopPrefixLemma(bs: BitStream, nBits: Int, i: Int, acc: Long): Unit = {
      require(0 <= i && i < nBits && nBits <= 64)
      require(BitStream.validate_offset_bits(bs.buf.length.toLong, bs.currentByte.toLong, bs.currentBit.toLong, nBits - i))
      require((acc & onesLSBLong(nBits - i)) == 0L)
      require((acc & onesLSBLong(nBits)) == acc)
      decreases(nBits - i)
      val (bsFinal, vGot1) = bs.readNLSBBitsMSBFirstLoopPure(nBits, i, acc)
      val readBit = bs.readBitPure()._2
      val bs2 = bs.withMovedBitIndex(1)
      val newAcc = acc | (if readBit then 1L << (nBits - 1 - i) else 0)
      val (bs2Final, vGot2) = bs2.readNLSBBitsMSBFirstLoopPure(nBits, i + 1, newAcc)

      {
         ()
      }.ensuring { _ =>
         vGot1 == vGot2 && bsFinal == bs2Final
      }
   }

   @ghost @pure @opaque @inlineOnce
   def readNLSBBitsMSBFirstLoopPrefixLemma2(bs1: BitStream, bs2: BitStream, nBits: Int, i: Int, acc: Long): Unit = {
      require(bs1.buf.length == bs2.buf.length)
      require(0 <= i && i < nBits && nBits <= 64)
      require(BitStream.validate_offset_bits(bs1.buf.length.toLong, bs1.currentByte.toLong, bs1.currentBit.toLong, nBits - i))
      require((acc & onesLSBLong(nBits - i)) == 0L)
      require((acc & onesLSBLong(nBits)) == acc)
      require(arrayBitRangesEq(
         bs1.buf,
         bs2.buf,
         0,
         BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) + nBits - i
      ))
      decreases(nBits - i)

      val bs2Reset = bs2.resetAt(bs1)
      val (bsFinal1, vGot1) = bs1.readNLSBBitsMSBFirstLoopPure(nBits, i, acc)
      val (bsFinal2, vGot2) = bs2Reset.readNLSBBitsMSBFirstLoopPure(nBits, i, acc)

      {
         val (bs1Rec, gotB1) = bs1.readBitPure()
         val (bs2Rec, gotB2) = bs2Reset.readBitPure()
         arrayBitRangesEqSlicedLemma(bs1.buf, bs2.buf, 0, BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) + nBits - i, 0, BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) + 1)
         readBitPrefixLemma(bs1, bs2)
         assert(gotB1 == gotB2)
         if (i == nBits - 1) {
            check(vGot1 == vGot2)
            check(BitStream.bitIndex(bsFinal1.buf.length, bsFinal1.currentByte, bsFinal1.currentBit ) == BitStream.bitIndex(bsFinal2.buf.length, bsFinal2.currentByte, bsFinal2.currentBit ))
         } else {
            val accRec = acc | (if gotB1 then 1L << (nBits - 1 - i) else 0)
            assert(BitStream.bitIndex(bs1Rec.buf.length, bs1Rec.currentByte, bs1Rec.currentBit ) == BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) + 1)
            validateOffsetBitsContentIrrelevancyLemma(bs1, bs1Rec.buf, 1)
            readNLSBBitsMSBFirstLoopPrefixLemma2(bs1Rec, bs2Rec, nBits, i + 1, accRec)
            val (_, vRecGot1) = bs1Rec.readNLSBBitsMSBFirstLoopPure(nBits, i + 1, accRec)
            val (_, vRecGot2) = bs2Rec.readNLSBBitsMSBFirstLoopPure(nBits, i + 1, accRec)
            assert(vRecGot1 == vRecGot2)
            assert(vGot1 == vRecGot1)
            assert(vGot2 == vRecGot2)

            check(vGot1 == vGot2)
            check(BitStream.bitIndex(bsFinal1.buf.length, bsFinal1.currentByte, bsFinal1.currentBit ) == BitStream.bitIndex(bsFinal2.buf.length, bsFinal2.currentByte, bsFinal2.currentBit ))
         }
      }.ensuring { _ =>
         vGot1 == vGot2 && BitStream.bitIndex(bsFinal1.buf.length, bsFinal1.currentByte, bsFinal1.currentBit ) == BitStream.bitIndex(bsFinal2.buf.length, bsFinal2.currentByte, bsFinal2.currentBit )
      }
   }

   @ghost @pure @opaque @inlineOnce
   def readNLSBBitsMSBFirstPrefixLemma(bs1: BitStream, bs2: BitStream, nBits: Int): Unit = {
      require(bs1.buf.length == bs2.buf.length)
      require(0 <= nBits && nBits <= 64)
      require(BitStream.validate_offset_bits(bs1.buf.length.toLong, bs1.currentByte.toLong, bs1.currentBit.toLong, nBits))
      require(arrayBitRangesEq(
         bs1.buf,
         bs2.buf,
         0,
         BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) + nBits
      ))

      val bs2Reset = bs2.resetAt(bs1)
      val (bsFinal1, vGot1) = bs1.readNLSBBitsMSBFirstPure(nBits)
      val (bsFinal2, vGot2) = bs2Reset.readNLSBBitsMSBFirstPure(nBits)

      {
         if (nBits > 0)
            readNLSBBitsMSBFirstLoopPrefixLemma2(bs1, bs2, nBits, 0, 0)
      }.ensuring { _ =>
         vGot1 == vGot2 && BitStream.bitIndex(bsFinal1.buf.length, bsFinal1.currentByte, bsFinal1.currentBit ) == BitStream.bitIndex(bsFinal2.buf.length, bsFinal2.currentByte, bsFinal2.currentBit )
      }
   }

   @ghost @pure @opaque @inlineOnce
   def readNLSBBitsMSBFirstLoopNextLemma(bs: BitStream, nBits: Int, i: Int, acc1: Long): Unit = {
      require(0 <= i && i < nBits && nBits <= 64)
      require(1 <= nBits)
      require(BitStream.validate_offset_bits(bs.buf.length.toLong, bs.currentByte.toLong, bs.currentBit.toLong, nBits - i))
      require((acc1 & onesLSBLong(nBits - i)) == 0L)
      require((acc1 & onesLSBLong(nBits)) == acc1)
      decreases(nBits - i)

      val (bsFinal1, vGot1) = bs.readNLSBBitsMSBFirstLoopPure(nBits, i, acc1)
      val (bs2, bit) = bs.readBitPure()
      val mask = if bit then 1L << (nBits - 1 - i) else 0
      val acc2 = (acc1 | mask) & onesLSBLong(nBits - 1)
      val (bsFinal2, vGot2) = bs2.readNLSBBitsMSBFirstLoopPure(nBits - 1, i, acc2)

      {
         if (i >= nBits - 2) ()
         else {
            val acc1Rec = acc1 | mask
            readNLSBBitsMSBFirstLoopNextLemma(bs2, nBits, i + 1, acc1Rec)
            val (bsFinal1Rec, vGot1Rec) = bs2.readNLSBBitsMSBFirstLoopPure(nBits, i + 1, acc1Rec)
            val (bs2Rec, bitRec) = bs2.readBitPure()
            val maskRec = if bitRec then 1L << (nBits - 2 - i) else 0
            val acc2Rec = (acc1Rec | maskRec) & onesLSBLong(nBits - 1)
            val (bsFinal2Rec, vGot2Rec) = bs2Rec.readNLSBBitsMSBFirstLoopPure(nBits - 1, i + 1, acc2Rec)
            assert((vGot1Rec & onesLSBLong(nBits - 1)) == vGot2Rec)
            assert(bsFinal1Rec == bsFinal2Rec)

            assert(bsFinal2 == bsFinal1Rec)
            assert(BitStream.bitIndex(bsFinal1.buf.length, bsFinal1.currentByte, bsFinal1.currentBit ) == BitStream.bitIndex(bsFinal1Rec.buf.length, bsFinal1Rec.currentByte, bsFinal1Rec.currentBit))
            assert(bsFinal1.buf == bsFinal1Rec.buf)
            eqBufAndBitIndexImpliesEq(bsFinal1, bsFinal1Rec)
            check(bsFinal1 == bsFinal2)

            assert(vGot1 == (vGot1Rec | mask))
            check((vGot1 & onesLSBLong(nBits - 1)) == vGot2)
         }
      }.ensuring { _ =>
         (vGot1 & onesLSBLong(nBits - 1)) == vGot2 && bsFinal1 == bsFinal2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def readNLSBBitsMSBFirstLeadingZerosLemma(bs: BitStream, nBits: Int, leadingZeros: Int): Unit = {
      require(0 <= leadingZeros && leadingZeros <= nBits && nBits <= 64)
      require(BitStream.validate_offset_bits(bs.buf.length.toLong, bs.currentByte.toLong, bs.currentBit.toLong, nBits))
      require(bs.readNLSBBitsMSBFirstPure(leadingZeros)._2 == 0L)
      decreases(leadingZeros)

      val (bsFinal1, vGot1) = bs.readNLSBBitsMSBFirstPure(nBits)
      val (bsFinal2, vGot2) = bs.withMovedBitIndex(leadingZeros).readNLSBBitsMSBFirstPure(nBits - leadingZeros)

      {
         readNLSBBitsMSBFirstLeadingBitsLemma(bs, false, nBits, leadingZeros)
      }.ensuring { _ =>
         vGot1 == vGot2 && bsFinal1 == bsFinal2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def readNLSBBitsMSBFirstLeadingBitsLemma(bs: BitStream, bit: Boolean, nBits: Int, leadingBits: Int): Unit = {
      require(0 <= leadingBits && leadingBits <= nBits && nBits <= 64)
      require(BitStream.validate_offset_bits(bs.buf.length.toLong, bs.currentByte.toLong, bs.currentBit.toLong, nBits))
      require(bs.readNLSBBitsMSBFirstPure(leadingBits)._2 == bitLSBLong(bit, leadingBits))
      decreases(leadingBits)

      val (bsFinal1, vGot1) = bs.readNLSBBitsMSBFirstPure(nBits)
      val (bsFinal2, vGot2) = bs.withMovedBitIndex(leadingBits).readNLSBBitsMSBFirstPure(nBits - leadingBits)

      {
         if (leadingBits == 0) ()
         else {
            val (bsRec, gotBit) = bs.readBitPure()
            assert(gotBit == bit)
            readNLSBBitsMSBFirstLoopNextLemma(bs, leadingBits, 0, 0L)
            readNLSBBitsMSBFirstLeadingBitsLemma(bsRec, bit, nBits - 1, leadingBits - 1)
            eqBufAndBitIndexImpliesEq(bs.withMovedBitIndex(leadingBits), bsRec.withMovedBitIndex(leadingBits - 1))

            val (bsFinal1Rec, vGot1Rec) = bsRec.readNLSBBitsMSBFirstPure(nBits - 1)
            val (bsFinal2Rec, vGot2Rec) = bsRec.withMovedBitIndex(leadingBits - 1).readNLSBBitsMSBFirstPure(nBits - leadingBits)
            assert(bsFinal1Rec == bsFinal2Rec)
            assert(vGot1Rec == ((bitLSBLong(bit, leadingBits - 1) << (nBits - leadingBits)) | vGot2Rec))
            assert(bsFinal2 == bsFinal2Rec)
            assert(vGot2 == vGot2Rec)

            readNLSBBitsMSBFirstLoopNextLemma(bs, nBits, 0, 0L)
            assert(bsFinal1Rec == bsFinal1)
            assert(vGot1 == (vGot1Rec | (if (bit) 1L << (nBits - 1) else 0L)))
            check(vGot1 == ((bitLSBLong(bit, leadingBits) << (nBits - leadingBits)) | vGot2))
            check(bsFinal1 == bsFinal2)
         }
      }.ensuring { _ =>
         vGot1 == ((bitLSBLong(bit, leadingBits) << (nBits - leadingBits)) | vGot2) && bsFinal1 == bsFinal2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def checkBitsLoopAndReadNLSB(bs: BitStream, nBits: Int, bit: Boolean, from: Int = 0): Unit = {
      require(0 < nBits && nBits <= 64)
      require(0 <= from && from <= nBits)
      require(BitStream.validate_offset_bits(bs.buf.length.toLong, bs.currentByte.toLong, bs.currentBit.toLong, nBits - from))
      decreases(nBits - from)
      val (bs1Final, ok) = bs.checkBitsLoopPure(nBits, bit, from)
      require(ok)
      val acc = if (bit) onesLSBLong(from) << (nBits - from) else 0
      val (bs2Final, vGot) = bs.readNLSBBitsMSBFirstLoopPure(nBits, from, acc)

      {
         if (from == nBits) ()
         else {
            val (bs1Rec, _) = bs.readBitPure()
            checkBitsLoopAndReadNLSB(bs1Rec, nBits, bit, from + 1)
         }
      }.ensuring { _ =>
         if (!bit) vGot == 0
         else vGot == onesLSBLong(nBits)
      }
   }

   // TODO: Bad name
   @ghost @pure @opaque @inlineOnce
   def checkBitsLoopPrefixLemma(bs: BitStream, nBits: Long, expected: Boolean, from: Long): Unit = {
      require(0 < nBits && nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      require(0 <= from && from < nBits)
      require(BitStream.validate_offset_bits(bs.buf.length.toLong, bs.currentByte.toLong, bs.currentBit.toLong, nBits - from))
      val (bsFinal, vGot1) = bs.checkBitsLoopPure(nBits, expected, from)
      val readBit = bs.readBitPure()._2
      val bs2 = bs.withMovedBitIndex(1)
      val (bs2Final, vGot2) = bs2.checkBitsLoopPure(nBits, expected, from + 1)

      {
         ()
      }.ensuring { _ =>
         // rewritten SAM
         vGot1 == ((readBit == expected) && vGot2)
         &&
         (if(readBit == expected) then (bsFinal == bs2Final) else true)

         // vGot1 == ((readBit == expected) && vGot2) && ((readBit == expected) ==> (bsFinal == bs2Final))
      }
   }

   @ghost @pure @opaque @inlineOnce
   def checkBitsLoopPrefixLemma2(bs1: BitStream, bs2: BitStream, nBits: Int, expected: Boolean, from: Long): Unit = {
      require(bs1.buf.length == bs2.buf.length)
      require(0 < nBits && nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      require(0 <= from && from < nBits)
      require(BitStream.validate_offset_bits(bs1.buf.length.toLong, bs1.currentByte.toLong, bs1.currentBit.toLong, nBits - from))
      require(arrayBitRangesEq(
         bs1.buf,
         bs2.buf,
         0,
         BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) + nBits - from
      ))
      decreases(nBits - from)

      val bs2Reset = bs2.resetAt(bs1)
      val (bsFinal1, vGot1) = bs1.checkBitsLoopPure(nBits, expected, from)
      val (bsFinal2, vGot2) = bs2Reset.checkBitsLoopPure(nBits, expected, from)

      val bsFinal1PureBitIndex = BitStream.bitIndex(bsFinal1.buf.length, bsFinal1.currentByte, bsFinal1.currentBit )
      val bsFinal2PureBitIndex = BitStream.bitIndex(bsFinal2.buf.length, bsFinal2.currentByte, bsFinal2.currentBit )

      {
         val (bs1Rec, gotB1) = bs1.readBitPure()
         val (bs2Rec, gotB2) = bs2Reset.readBitPure()
         arrayBitRangesEqSlicedLemma(bs1.buf, bs2.buf, 0, BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) + nBits - from, 0, BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) + 1)
         readBitPrefixLemma(bs1, bs2)
         assert(gotB1 == gotB2)
         if (from == nBits - 1) {
            check(vGot1 == vGot2)
            assert(BitStream.invariant(bsFinal1))
            check(BitStream.bitIndex(bsFinal1.buf.length, bsFinal1.currentByte, bsFinal1.currentBit ) == BitStream.bitIndex(bsFinal2.buf.length, bsFinal2.currentByte, bsFinal2.currentBit ))
         } else {
            assert(BitStream.invariant(bs1Rec))
            assert(BitStream.bitIndex(bs1Rec.buf.length, bs1Rec.currentByte, bs1Rec.currentBit ) == BitStream.bitIndex(bs1.buf.length, bs1.currentByte, bs1.currentBit ) + 1)
            validateOffsetBitsContentIrrelevancyLemma(bs1, bs1Rec.buf, 1)
            assert(BitStream.invariant(bs1Rec))
            assert((BitStream.validate_offset_bits(bs1Rec.buf.length.toLong, bs1Rec.currentByte.toLong, bs1Rec.currentBit.toLong, nBits - from - 1)))
            checkBitsLoopPrefixLemma2(bs1Rec, bs2Rec, nBits, expected, from + 1)

            val (_, vRecGot1) = bs1Rec.checkBitsLoopPure(nBits, expected, from + 1)
            assert((BitStream.validate_offset_bits(bs2Rec.buf.length.toLong, bs2Rec.currentByte.toLong, bs2Rec.currentBit.toLong, nBits - from - 1)))
            val (_, vRecGot2) = bs2Rec.checkBitsLoopPure(nBits, expected, from + 1)

            assert(vRecGot1 == vRecGot2)
            assert(vGot1 == ((gotB1 == expected) && vRecGot1))
            assert(vGot2 == ((gotB1 == expected) && vRecGot2))

            check(vGot1 == vGot2)
            assert(BitStream.invariant(bsFinal2.currentBit, bsFinal2.currentByte, bsFinal2.buf.length))
            assert(BitStream.invariant(bsFinal1.currentBit, bsFinal1.currentByte, bsFinal1.buf.length))
            assert(bsFinal2PureBitIndex == BitStream.bitIndex(bsFinal2.buf.length, bsFinal2.currentByte, bsFinal2.currentBit ))
            assert(BitStream.bitIndex(bsFinal1.buf.length, bsFinal1.currentByte, bsFinal1.currentBit ) == bsFinal1PureBitIndex)
            assert(BitStream.bitIndex(bsFinal1.buf.length, bsFinal1.currentByte, bsFinal1.currentBit ) == BitStream.bitIndex(bsFinal2.buf.length, bsFinal2.currentByte, bsFinal2.currentBit )) // 200sec!!!
            check(BitStream.bitIndex(bsFinal1.buf.length, bsFinal1.currentByte, bsFinal1.currentBit ) == BitStream.bitIndex(bsFinal2.buf.length, bsFinal2.currentByte, bsFinal2.currentBit ))
         }
      }.ensuring { _ =>
         vGot1 == vGot2 && BitStream.bitIndex(bsFinal1.buf.length, bsFinal1.currentByte, bsFinal1.currentBit ) == BitStream.bitIndex(bsFinal2.buf.length, bsFinal2.currentByte, bsFinal2.currentBit )
      }
   }

   // @ghost @pure @opaque @inlineOnce
   // def readByteArrayLoopAnyArraysLemma(bs: BitStream, arr1: Array[UByte], arr2: Array[UByte], from: Int, to: Int): Unit = {
   //    require(arr1.length <= arr2.length)
   //    require(0 <= from && from <= to && to <= arr1.length)
   //    require( BitStream.validate_offset_bytes(bs.buf.length.toLong, bs.currentByte.toLong, bs.currentBit.toLong,to - from))
   //    decreases(to - from)

   //    val (_, arr1b) = bs.readByteArrayLoopPure(arr1, from, to)
   //    val (_, arr2b) = bs.readByteArrayLoopPure(arr2, from, to)

   //    {
   //       if (from == to) {
   //          ()
   //       } else {
   //          val bsRec = bs.withMovedByteIndex(1)
   //          val b = bs.readBytePure()._2
   //          validateOffsetBytesFromBitIndexLemma(bs, bsRec, 8, to - from)
   //          readByteArrayLoopAnyArraysLemma(bsRec, arr1.updated(from, b), arr2.updated(from, b), from + 1, to)
   //       }
   //    }.ensuring(_ => arrayRangesEq(arr1b, arr2b, from, to))
   // }

   @ghost @pure @opaque @inlineOnce
   def readByteArrayLoopArrayPrefixLemma(bs: BitStream, arr: Array[UByte], from: Int, to: Int): Unit = {
      require(0 <= from && from < to && to <= arr.length)
      require( BitStream.validate_offset_bytes(bs.buf.length.toLong, bs.currentByte.toLong, bs.currentBit.toLong,to - from))
      decreases(to - from)
      val (_, arr1) = bs.readByteArrayLoopPure(arr, from, to)
      val bs2 = bs.withMovedByteIndex(1)
      val (_, arr2) = bs2.readByteArrayLoopPure(arr.updated(from, bs.readBytePure()._2), from + 1, to)

      {
         if (from == to - 1) {
            ()
         } else {
            val bsRec = bs.withMovedByteIndex(1)
            val b1 = bs.readBytePure()._2
            val b2 = bs2.readBytePure()._2
            val arr_rec = arr.updated(from, b1)
            validateOffsetBytesFromBitIndexLemma(bs, bsRec, 8, to - from)
            readByteArrayLoopArrayPrefixLemma(bsRec, arr_rec, from + 1, to)
         }
      }.ensuring { _ =>
         arrayRangesEq(arr1, arr2, 0, to)
      }
   }

   // TODO: Not proved
   @extern
   @ghost @pure @opaque @inlineOnce
   def readBitsVecPrefixLemma(bs1: BitStream, bs2: BitStream, nBits: Long): Unit = {
      require(bs1.buf.length == bs2.buf.length)
      require(0 <= nBits && nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      require(bs1.validate_offset_bits(nBits))
      require(arrayBitRangesEq(
         bs1.buf,
         bs2.buf,
         0,
         bs1.bitIndex + nBits
      ))

      val bs2Reset = BitStream(snapshot(bs2.buf), bs1.currentByte, bs1.currentBit)
      val (bs1Res, v1) = bs1.readBitsVecPure(nBits)
      val (bs2Res, v2) = bs2Reset.readBitsVecPure(nBits)

      {
         ()
      }.ensuring { _ =>
         bs1Res.bitIndex == bs2Res.bitIndex && v1 == v2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def lemmaIsPrefixRefl(bs: BitStream): Unit = {
      if (bs.buf.length != 0) {
         arrayBitEqImpliesRangesEqLemma(bs.buf)
         arrayBitRangesEqSlicedLemma(bs.buf, snapshot(bs.buf), 0, bs.buf.length.toLong * 8, 0, BitStream.bitIndex(bs.buf.length, bs.currentByte, bs.currentBit ))
      }
   }.ensuring { _ =>
      bs.isPrefixOf(snapshot(bs))
   }

   @ghost @pure @opaque @inlineOnce
   def lemmaIsPrefixTransitive(w1: BitStream, w2: BitStream, w3: BitStream): Unit = {
      require(w1.isPrefixOf(w2))
      require(w2.isPrefixOf(w3))
      arrayRangesEqTransitive(w1.buf, w2.buf, w3.buf, 0, w1.currentByte, w2.currentByte)
      if (w1.currentByte < w1.buf.length) {
         if (w1.currentByte < w2.currentByte) {
            arrayRangesEqImpliesEq(w2.buf, w3.buf, 0, w1.currentByte, w2.currentByte)
            assert(w2.buf(w1.currentByte) == w3.buf(w1.currentByte))
            check(byteRangesEq(w1.buf(w1.currentByte), w3.buf(w1.currentByte), 0, w1.currentBit))
         } else {
            assert(w1.currentBit <= w2.currentBit)
            check(byteRangesEq(w1.buf(w1.currentByte), w3.buf(w1.currentByte), 0, w1.currentBit))
         }
      }
      assert(((w1.currentByte < w1.buf.length) ==> byteRangesEq(w1.buf(w1.currentByte), w3.buf(w1.currentByte), 0, w1.currentBit)))
      check(w1.isPrefixOf(w3))
   }.ensuring { _ =>
      w1.isPrefixOf(w3)
   }

   def moveByteIndexPrecond(b: BitStream, diffInBytes: Int): Boolean = {
      -b.buf.length <= diffInBytes && diffInBytes <= b.buf.length && {
         val res = b.currentByte.toLong + diffInBytes.toLong
         0 <= res && (res < b.buf.length) || (b.currentBit == 0 && res == b.buf.length)
      }
   }
   def moveBitIndexPrecond(b: BitStream, diffInBits: Long): Boolean = {
      // This condition ensures we do not have an overflow in `res`, should always hold and is easier to verify than the general condition for no overflow
      -8 * b.buf.length.toLong <= diffInBits && diffInBits <= 8 * b.buf.length.toLong && {
         val res = BitStream.bitIndex(b.buf.length, b.currentByte, b.currentBit ) + diffInBits
         0 <= res && res <= 8 * b.buf.length.toLong
      }
   }
}

case class BitStream private [asn1scala](
                       var buf: Array[Byte],
                       var currentByte: Int = 0, // marks the currentByte that gets accessed
                       var currentBit: Int = 0,  // marks the next bit that gets accessed
                    ) { // all BisStream instances satisfy the following:
   import BitStream.*
   require(BitStream.invariant(currentBit, currentByte, buf.length))

   @pure
   def isPrefixOf(b2: BitStream): Boolean = {
      buf.length == b2.buf.length &&
      BitStream.bitIndex(buf.length, currentByte, currentBit) <= BitStream.bitIndex(b2.buf.length, b2.currentByte, b2.currentBit ) &&
      (buf.length != 0) ==> arrayBitRangesEq(buf, b2.buf, 0, BitStream.bitIndex(buf.length, currentByte, currentBit))
   }

   def resetBitIndex(): Unit = {
      currentBit = 0
      currentByte = 0
   }

   private def increaseBitIndex(): Unit = {
      require(BitStream.remainingBits(buf.length.toLong, currentByte.toLong, currentBit.toLong) > 0)
      if currentBit < NO_OF_BITS_IN_BYTE - 1 then
         currentBit += 1
      else
         currentBit = 0
         currentByte += 1

   }.ensuring {_ =>
      val oldBitStr = old(this)
      BitStream.bitIndex(oldBitStr.buf.length, oldBitStr.currentByte, oldBitStr.currentBit) + 1 == BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit ) &&
      BitStream.remainingBits(oldBitStr.buf.length.toLong, oldBitStr.currentByte.toLong, oldBitStr.currentBit.toLong) - BitStream.remainingBits(buf.length.toLong, currentByte.toLong, currentBit.toLong) == 1 &&
      oldBitStr.buf.length == buf.length
   }

   def moveBitIndex(diffInBits: Long): Unit = {
      require(moveBitIndexPrecond(this, diffInBits))
      val nbBytes = (diffInBits / 8).toInt
      val nbBits = (diffInBits % 8).toInt
      currentByte += nbBytes
      if currentBit + nbBits < 0 then
        currentByte -= 1
        currentBit = 8 + nbBits + currentBit
      else if currentBit + nbBits >= 8 then
        currentBit = currentBit + nbBits - 8
        currentByte += 1
      else
        currentBit += nbBits
   }.ensuring(_ =>
      BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit ) + diffInBits == BitStream.bitIndex(buf.length, currentByte, currentBit)
      && old(this).buf.length == buf.length
      && old(this).buf == this.buf
   )

   @ghost @pure
   def withMovedBitIndex(diffInBits: Long): BitStream = {
      require(moveBitIndexPrecond(this, diffInBits))
      val cpy = snapshot(this)
      cpy.moveBitIndex(diffInBits)
      cpy
   }.ensuring(res =>
      BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit ) + diffInBits == BitStream.bitIndex(res.buf.length, res.currentByte, res.currentBit)
      && this.buf.length == res.buf.length
   )

   def moveByteIndex(diffInBytes: Int): Unit = {
      require(moveByteIndexPrecond(this, diffInBytes))
      currentByte += diffInBytes
   }.ensuring(_ => old(this).currentByte + diffInBytes == this.currentByte)

   @ghost @pure
   def withMovedByteIndex(diffInBytes: Int): BitStream = {
      require(moveByteIndexPrecond(this, diffInBytes))
      val cpy = snapshot(this)
      cpy.moveByteIndex(diffInBytes)
      cpy
   }

   @pure @inline
   def getBufferLength: Int = buf.length

   /**
    * Return count of bytes that got already fully or partially written
    *
    * @return the number of used bytes so far
    *
    * Example:
    *    Currentbyte = 4, currentBit = 2 --> 5
    *    Currentbyte = 14, currentBit = 0 --> 14
    *
    */
   @pure @inline
   def getLength: Int = {
      var ret: Int = currentByte
      if currentBit > 0 then
         ret += 1
      ret
   }

   @ghost @pure @inline
   def getBuf: Array[Byte] = buf

   @pure @inline
   def bitIndex: Long = BitStream.bitIndex(buf.length, currentByte, currentBit)

   @pure @inline
   def validate_offset_bits(bits: Long): Boolean = {
      require(0 <= bits)
      BitStream.validate_offset_bits(buf.length, currentByte, currentBit, bits)
   }

   @pure @inline
   def validate_offset_bytes(bytes: Int): Boolean = {
      require(bytes >= 0)
      BitStream.validate_offset_bytes(buf.length, currentByte, currentBit, bytes)
   }

   @ghost @pure @inline
   def resetAt(b: BitStream): BitStream = {
      require(b.buf.length == buf.length)
      BitStream(snapshot(buf), b.currentByte, b.currentBit)
   }.ensuring(res => res.buf == this.buf && res.currentByte == b.currentByte && res.currentBit == b.currentBit)

   // ****************** Append Bit Functions **********************

   /**
    * Append the bit b into the stream
    *
    * @param b bit that gets set
    *
    * Example
    * cur bit = 3
    *
    * |x|x|x|b|?|?|?|?|
    *  0 1 2 3 4 5 6 7
    *
    */
   @opaque @inlineOnce
   def appendBit(b: Boolean): Unit = {
      require(BitStream.validate_offset_bit(buf.length.toLong, currentByte.toLong, currentBit.toLong))

      @ghost val oldThis = snapshot(this)

      if b then
         buf(currentByte) = (buf(currentByte) | BitAccessMasks(currentBit)).toByte
      else
         buf(currentByte) = (buf(currentByte) & (~BitAccessMasks(currentBit))).toByte

      ghostExpr {
         arrayUpdatedAtPrefixLemma(oldThis.buf, currentByte, buf(currentByte))
      }

      increaseBitIndex()

   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      w1.buf.length == w2.buf.length && BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + 1
      && w1.isPrefixOf(w2)
      && {
         val r = readerFrom(w2, w1.currentBit, w1.currentByte)
         val (r2Got, bGot) = r.readBitPure()
         bGot == b
         && r2Got == this &&
         r2Got.bitIndex == this.bitIndex // obvious but important as documentation
      }
   }

   /**
    * Append a set bit
    */
   @opaque @inlineOnce
   def appendBitOne(): Unit = {
      require(BitStream.validate_offset_bit(buf.length.toLong, currentByte.toLong, currentBit.toLong))

      appendBit(true)
   }.ensuring(_ =>
      val w1 = old(this)
      val w2 = this
      w2.buf.length == w1.buf.length &&
      BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit) + 1
      && w1.isPrefixOf(w2)
      && {
         val r = readerFrom(w2, w1.currentBit, w1.currentByte)
         val (r2Got, bGot) = r.readBitPure()
         bGot == true
         && r2Got == this &&
         r2Got.bitIndex == this.bitIndex
      }
   )

   /**
    * Append cleared bit to bitstream
    */
    @opaque @inlineOnce
   def appendBitZero(): Unit = {
      require(BitStream.validate_offset_bit(buf.length.toLong, currentByte.toLong, currentBit.toLong))

      appendBit(false)
   }.ensuring(_ =>
      val w1 = old(this)
      val w2 = this
      w2.buf.length == w1.buf.length &&
      BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit) + 1
      && w1.isPrefixOf(w2)
      && {
         val r = readerFrom(w2, w1.currentBit, w1.currentByte)
         val (r2Got, bGot) = r.readBitPure()
         bGot == false
         && r2Got == this &&
         r2Got.bitIndex == this.bitIndex
      }
   )

   @opaque @inlineOnce
   def appendNBits(nBits: Long, bit: Boolean): Unit = {
      require(0 <= nBits && nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))
      appendNBitsLoop(nBits, bit, 0)
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      w1.buf.length == w2.buf.length
      && BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit ) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + nBits
      && w1.isPrefixOf(w2)
      && {
         val (r1, r2) = reader(w1, w2)
         validateOffsetBitsContentIrrelevancyLemma(w1, w2.buf, nBits)
         val (r2Got, bGot) = r1.checkBitsLoopPure(nBits, bit, 0)
         bGot && r2Got == r2
      }
   }

   @opaque @inlineOnce
   def appendNBitsLoop(nBits: Long, bit: Boolean, from: Long): Unit = {
      require(0 <= nBits && nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      require(0 <= from && from <= nBits)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits - from))
      decreases(nBits - from)
      @ghost val oldThis = snapshot(this)
      if (from < nBits) {
         @ghost val oldThis1 = snapshot(this)
         appendBit(bit)
         @ghost val oldThis2 = snapshot(this)
         ghostExpr {
           BitStream.validateOffsetBitsIneqLemma(oldThis1, this, nBits - from, 1)
         }
         appendNBitsLoop(nBits, bit, from + 1)
         ghostExpr {
            lemmaIsPrefixTransitive(oldThis1, oldThis2, this)
            readBitPrefixLemma(oldThis2.resetAt(oldThis1), this)

            check(BitStream.bitIndex(oldThis2.buf.length, oldThis2.currentByte, oldThis2.currentBit ) == BitStream.bitIndex(oldThis1.buf.length, oldThis1.currentByte, oldThis1.currentBit) + 1)

            val (r1_13, r3_13) = reader(oldThis1, this)
            val (r2_23, r3_23) = reader(oldThis2, this)
            val (_, bitGot) = r1_13.readBitPure()
            check(bitGot == bit)

            check(r2_23 == r1_13.withMovedBitIndex(1))

            validateOffsetBitsContentIrrelevancyLemma(oldThis1, this.buf, nBits - from)
            val (r3Got_13, resGot_13) = r1_13.checkBitsLoopPure(nBits, bit, from)

            validateOffsetBitsContentIrrelevancyLemma(oldThis2, this.buf, nBits - from - 1)
            val (r3Got_23, resGot_23) = r2_23.checkBitsLoopPure(nBits, bit, from + 1)

            assert(r3Got_23 == r3_23)

            // checkBitsLoopPrefixLemma(r1_13, nBits, bit, from) // not needed but speed up verification
            check(resGot_23)
            assert(r2_23 == r1_13.withMovedBitIndex(1))
            check(resGot_13 == resGot_23) // timeout
            check(r3Got_13 == r3_13)
            check(resGot_13)

         }

      } else {
         ghostExpr {
            lemmaIsPrefixRefl(this)

            assert(nBits == from)
         }
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      w1.buf.length == w2.buf.length
      &&& BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + (nBits - from)
      &&& w1.isPrefixOf(w2)
      &&& {
         val (r1, r2) = reader(w1, w2)
         validateOffsetBitsContentIrrelevancyLemma(w1, w2.buf, nBits - from)
         val (r2Got, bGot) = r1.checkBitsLoopPure(nBits, bit, from)
         bGot && r2Got == r2
      }
   }

   /**
    * Append n set bits to bitstream
    *
    * @param nBits number of bits
    *
    */

   def appendNOneBits(nBits: Long): Unit = {
      require(0 <= nBits && nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))
      appendNBits(nBits, true)
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      w1.buf.length == w2.buf.length
      && BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + nBits
      && w1.isPrefixOf(w2)
      && {
         val (r1, r2) = reader(w1, w2)
         validateOffsetBitsContentIrrelevancyLemma(w1, w2.buf, nBits)
         val (r2Got, bGot) = r1.checkBitsLoopPure(nBits, true, 0)
         bGot && r2Got == r2
      }
   }

   /**
    * Append n cleared bits to bitstream
    *
    * @param nBits number of bits
    *
    */
   def appendNZeroBits(nBits: Long): Unit = {
      require(0 <= nBits && nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))
      appendNBits(nBits, false)
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      w1.buf.length == w2.buf.length
      && BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + nBits
      && w1.isPrefixOf(w2)
      && {
         val (r1, r2) = reader(w1, w2)
         validateOffsetBitsContentIrrelevancyLemma(w1, w2.buf, nBits)
         val (r2Got, bGot) = r1.checkBitsLoopPure(nBits, false, 0)
         bGot && r2Got == r2
      }
   }

   /**
    * Append bit with bitNr from b to bitstream
    *
    * @param b byte that gets the bit extracted from
    * @param bitNr 0 to 7 - number of the bit
    *
    * Remarks:
    * bit 0 is the MSB, bit 7 is the LSB, ESA declares bit 1 as MSB,
    * bit 8 as LSB - but we start from 0 in CS
    *
    */
   private def appendBitFromByte(b: Byte, bitNr: Int): Unit = {
      require(bitNr >= 0 && bitNr < NO_OF_BITS_IN_BYTE)
      require(BitStream.validate_offset_bit(buf.length.toLong, currentByte.toLong, currentBit.toLong))

      // val bitPosInByte = 1 << ((NO_OF_BITS_IN_BYTE - 1) - bitNr) // change to the following to match spec
      val mask = BitAccessMasks(bitNr)
      val bit = (b.toUByte & mask) != 0
      appendBit(bit)

   }.ensuring(_ =>
      val w1 = old(this)
      val w2 = this
      val mask = BitAccessMasks(bitNr)
      val bit = (b.toUByte & mask) != 0
      w2.buf.length == w1.buf.length &&
      BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit) + 1
      && w1.isPrefixOf(w2)
      && {
         val r = readerFrom(w2, w1.currentBit, w1.currentByte)
         val (r2Got, bGot) = r.readBitPure()
         val vGot = r.readBits(1)
         bGot == bit
         && r2Got == this &&
         r2Got.bitIndex == this.bitIndex
         // && byteArrayBitContentSame(Array(b.toUByte), vGot, bitNr, 0 , 1)

      }
   )

   /**
    * Append nBits from the 64bit Integer value v to the bitstream
    *
    * @param v source of the bits
    * @param nBits number of bits to add
    *
    * Remarks:
    * bit 0 is the LSB of v
    */
   @opaque @inlineOnce
   def appendBitsLSBFirstWhile(v: Long, nBits: Int): Unit = {
      require(nBits >= 0 && nBits <= NO_OF_BITS_IN_LONG)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))

      @ghost val oldThis = snapshot(this)
      assert(BitStream.invariant(this))
      assert(BitStream.invariant(currentBit, currentByte, buf.length))
      var i = 0
      (while i < nBits do
         decreases(nBits - i)

         val ii = v & (1L << i)
         val b = ii != 0

         appendBit(b)

         i += 1
         assert(BitStream.invariant(currentBit, currentByte, buf.length))
      ).invariant(
         i >= 0 &&
         BitStream.invariant(currentBit, currentByte, buf.length) && i <= nBits &&
         buf.length == oldThis.buf.length &&
         BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(oldThis.buf.length, oldThis.currentByte, oldThis.currentBit) + i &&
         BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits - i))
   }.ensuring(_ =>
      buf.length == old(this).buf.length &&
      BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit) + nBits

      )

   def appendBitsLSBFirst(v: Long, nBits: Int): Unit = {
      require(nBits >= 0 && nBits <= NO_OF_BITS_IN_LONG)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))

      @ghost val oldThis = snapshot(this)
      assert(BitStream.invariant(this))
      assert(BitStream.invariant(currentBit, currentByte, buf.length))

      appendBitsLSBFirstLoopTR(v, nBits, 0)
   }.ensuring(_ =>
      val w1 = old(this)
      val w2 = this
      buf.length == old(this).buf.length
      && BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit) + nBits
      && w1.buf.length == w2.buf.length
      && BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + nBits
      && w1.isPrefixOf(w2)
      && {
         val (r1, r2) = reader(w1, w2)
         validateOffsetBitsContentIrrelevancyLemma(w1, w2.buf, nBits)
         val (r2Got, vGot) = r1.readNBitsLSBFirstPure(nBits)
         vGot == (v & onesLSBLong(nBits)) && r2Got == r2
      }

      )

   def appendBitsLSBFirstLoopTR(v: Long, nBits: Int, i: Int): Unit = {
      require(nBits >= 0 && nBits <= NO_OF_BITS_IN_LONG)
      require(0 <= i && i <= nBits)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits - i))

      decreases(nBits - i)

      @ghost val oldThis = snapshot(this)
      assert(BitStream.invariant(this))
      assert(BitStream.invariant(currentBit, currentByte, buf.length))

      if(i == nBits) {
         assert(BitStream.invariant(currentBit, currentByte, buf.length) )
         assert(buf.length == oldThis.buf.length )
         assert(BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(oldThis.buf.length, oldThis.currentByte, oldThis.currentBit) + nBits - i )
         ghostExpr(lemmaIsPrefixRefl(this))
         assert(oldThis.isPrefixOf(this))
         ()
      } else {
         val ii = v & (1L << i)
         val b = ii != 0

         appendBit(b)

         @ghost val oldThis2 = snapshot(this)
         assert(BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(oldThis.buf.length, oldThis.currentByte, oldThis.currentBit) + 1 )
         assert(BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(oldThis2.buf.length, oldThis2.currentByte, oldThis2.currentBit) )
         assert(BitStream.bitIndex(oldThis2.buf.length, oldThis2.currentByte, oldThis2.currentBit) == BitStream.bitIndex(oldThis.buf.length, oldThis.currentByte, oldThis.currentBit) + 1 )

         val res = appendBitsLSBFirstLoopTR(v, nBits, i + 1)

         ghostExpr(lemmaIsPrefixTransitive(oldThis, oldThis2, this))

         assert(BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(oldThis2.buf.length, oldThis2.currentByte, oldThis2.currentBit) + nBits - i  - 1)

         assert(BitStream.invariant(currentBit, currentByte, buf.length) )
         assert(buf.length == oldThis.buf.length )
         assert(BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(oldThis.buf.length, oldThis.currentByte, oldThis.currentBit) + nBits - i )

         assert(oldThis2.isPrefixOf(this))
         assert(oldThis.isPrefixOf(oldThis2))

         ghostExpr({
            readBitPrefixLemma(oldThis2.resetAt(oldThis), this)

            val (r1_13, r3_13) = reader(oldThis, this)
            val (r2_23, r3_23) = reader(oldThis2, this)
            val (_, bitGot) = r1_13.readBitPure()
            check(bitGot == b)

            val zeroed = v & onesLSBLong(i)
            validateOffsetBitsContentIrrelevancyLemma(oldThis, this.buf, nBits - i)
            val (r3Got_13, resGot_13) = r1_13.readNBitsLSBFirstsLoopPure(nBits, i, zeroed)

            val upd = zeroed | (if bitGot then 1L << i else 0)
            validateOffsetBitsContentIrrelevancyLemma(oldThis2, this.buf, nBits - i - 1)
            val (r3Got_23, resGot_23) = r2_23.readNBitsLSBFirstsLoopPure(nBits, i + 1, upd)

            assert(r3Got_23 == r3_23)

            lemmaReadNBitsLSBFirstsLoopIsCorrect(r1_13, nBits, i, zeroed)

            check(r1_13 == r3_13.withMovedBitIndex(BitStream.bitIndex(oldThis.buf.length, oldThis.currentByte, oldThis.currentBit) - BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit) ))
            check(r2_23 == r3_23.withMovedBitIndex(BitStream.bitIndex(oldThis2.buf.length, oldThis2.currentByte, oldThis2.currentBit) - BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit) ))
            check(BitStream.bitIndex(oldThis.buf.length, oldThis.currentByte, oldThis.currentBit) == BitStream.bitIndex(oldThis2.buf.length, oldThis2.currentByte, oldThis2.currentBit) - 1)

            assert(r2_23 == r1_13.withMovedBitIndex(1))
            check(resGot_13 == resGot_23)
            assert(BitStream.bitIndex(r3Got_13.buf.length, r3Got_13.currentByte, r3Got_13.currentBit) == BitStream.bitIndex(r3_13.buf.length, r3_13.currentByte, r3_13.currentBit))


            // helps with the performance, otherwise it times out even with 600sec sometimes
            check(BitStream.invariant(currentBit, currentByte, buf.length) )
            check(oldThis.buf.length == this.buf.length )
            check(BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit) == BitStream.bitIndex(oldThis.buf.length, oldThis.currentByte, oldThis.currentBit) + nBits - i )
            check(oldThis.isPrefixOf(this) )
            check({
               val (r1, r2) = reader(oldThis, this)
               val zeroed = v & onesLSBLong(i)
               validateOffsetBitsContentIrrelevancyLemma(oldThis, this.buf, nBits - i)
               val (r2Got, vGot) = r1.readNBitsLSBFirstsLoopPure(nBits, i, zeroed)
               vGot == (v & onesLSBLong(nBits)) && r2Got == r2
            })
         })
         res
      }
   }.ensuring(_ =>
      val w1 = old(this)
      val w2 = this
      BitStream.invariant(currentBit, currentByte, buf.length)
      &&& w1.buf.length == w2.buf.length
      &&& BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + nBits - i
      &&& w1.isPrefixOf(w2)
      &&& {
         val (r1, r2) = reader(w1, w2)
         val zeroed = v & onesLSBLong(i)
         validateOffsetBitsContentIrrelevancyLemma(w1, w2.buf, nBits - i)
         val (r2Got, vGot) = r1.readNBitsLSBFirstsLoopPure(nBits, i, zeroed)
         vGot == (v & onesLSBLong(nBits)) && r2Got == r2
      }
   )

   /**
    * Append nBits from the 64bit Integer value v to the bitstream
    *
    * @param v     source of the bits
    * @param nBits number of bits to add
    *
    * Remarks:
    * The first bit added to the bitstream is the highest significant bit
    * defined by nBits. The last added bit is the LSB.
    *
    * Example:
    * nBits = 25
    *
    * MSB          first added bit     LSB
    *  v                 v              v
    * 63----------------24--------------0
    *
    * After bit 24, bit 23 and so on get added
    *
    */
   def appendLSBBitsMSBFirst(v: Long, nBits: Int): Unit = {
      require(nBits >= 0 && nBits <= NO_OF_BITS_IN_LONG)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))
      require((v & onesLSBLong(nBits)) == v)
      appendLSBBitsMSBFirstLoop(v, nBits, 0)
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      w1.buf.length == w2.buf.length && BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit ) + nBits
      && w1.isPrefixOf(w2) && {
         val (r1, r2) = reader(w1, w2)
         validateOffsetBitsContentIrrelevancyLemma(w1, w2.buf, nBits)
         val (r2Got, vGot) = r1.readNLSBBitsMSBFirstPure(nBits)
         vGot == v && r2Got == r2
      }
   }

   def appendLSBBitsMSBFirstLoop(v: Long, nBits: Int, i: Int): Unit = {
      require(0 <= i && i <= nBits && nBits <= 64)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits - i))
      require((v & onesLSBLong(nBits)) == v)
      decreases(nBits - i)
      if (i < nBits) {
         val ii = v & (1L << (nBits - 1 - i))
         val b = ii != 0
         @ghost val oldThis1 = snapshot(this)
         appendBit(b)
         @ghost val oldThis2 = snapshot(this)
         appendLSBBitsMSBFirstLoop(v, nBits, i + 1)

         ghostExpr {
            lemmaIsPrefixTransitive(oldThis1, oldThis2, this)
            readBitPrefixLemma(oldThis2.resetAt(oldThis1), this)

            val (r1_13, r3_13) = reader(oldThis1, this)
            val (r2_23, r3_23) = reader(oldThis2, this)
            val (_, bitGot) = r1_13.readBitPure()
            check(bitGot == b)

            val zeroed = v & ~onesLSBLong(nBits - i)
            validateOffsetBitsContentIrrelevancyLemma(oldThis1, this.buf, nBits - i)
            val (r3Got_13, resGot_13) = r1_13.readNLSBBitsMSBFirstLoopPure(nBits, i, zeroed)

            val upd = zeroed | (if bitGot then 1L << (nBits - 1 - i) else 0)
            validateOffsetBitsContentIrrelevancyLemma(oldThis2, this.buf, nBits - i - 1)
            val (r3Got_23, resGot_23) = r2_23.readNLSBBitsMSBFirstLoopPure(nBits, i + 1, upd)

            assert(r3Got_23 == r3_23)

            readNLSBBitsMSBFirstLoopPrefixLemma(r1_13, nBits, i, zeroed)

            check(r1_13 == r3_13.withMovedBitIndex(BitStream.bitIndex(oldThis1.buf.length, oldThis1.currentByte, oldThis1.currentBit) - BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit) ))
            check(r2_23 == r3_23.withMovedBitIndex(BitStream.bitIndex(oldThis2.buf.length, oldThis2.currentByte, oldThis2.currentBit) - BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit) ))
            check(BitStream.bitIndex(oldThis1.buf.length, oldThis1.currentByte, oldThis1.currentBit) == BitStream.bitIndex(oldThis2.buf.length, oldThis2.currentByte, oldThis2.currentBit) - 1)

            assert(r2_23 == r1_13.withMovedBitIndex(1)) // really slow ~250sec
            check(resGot_13 == resGot_23)
            check(r3Got_13 == r3_13)
         }
      } else {
         ghostExpr {
            lemmaIsPrefixRefl(this)
         }
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      w1.buf.length == w2.buf.length && BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit ) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit ) + (nBits - i)
      && w1.isPrefixOf(w2)
      && {
         val (r1, r2) = reader(w1, w2)
         val zeroed = v & ~onesLSBLong(nBits - i)
         validateOffsetBitsContentIrrelevancyLemma(w1, w2.buf, nBits - i)
         val (r2Got, vGot) = r1.readNLSBBitsMSBFirstLoopPure(nBits, i, zeroed)
         vGot == v && r2Got == r2
      }
   }



    def appendBitsMSBFirstLoop(srcBuffer: Array[UByte], i: Long, to: Long): Unit = {
      require(to >= 0)
      require(i >= 0)
      require(i <= to)
      require(i < Long.MaxValue - to)
      require(i < Long.MaxValue)
      require(to < Long.MaxValue)
      require(to <= srcBuffer.length.toLong * 8L)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, to - i))
      decreases(to - i)

      @ghost val beforeAppend = snapshot(this)
      @ghost val oldSrcBuffer = snapshot(srcBuffer)
      @ghost val bitIndexBeforeAppend = BitStream.bitIndex(buf.length, currentByte, currentBit)
      ghostExpr(BitStream.lemmaIsPrefixRefl(this))
      if i < to then
         appendBitFromByte(srcBuffer((i / NO_OF_BITS_IN_BYTE).toInt).toRaw, (i % NO_OF_BITS_IN_BYTE).toInt)
         @ghost val afterAppend = snapshot(this)
         ghostExpr {
            BitStream.validateOffsetBitsIneqLemma(beforeAppend, this, to - i, 1)
         }

         assert(beforeAppend.isPrefixOf(this))
         assert(beforeAppend.isPrefixOf(afterAppend))

         ghostExpr({
            val (readerFromStartBeforeLoop, _) = reader(beforeAppend, this)
            validateOffsetBitsContentIrrelevancyLemma(beforeAppend, this.buf, 1)

            val listOfBitsFromStartBeforeLoop = bitStreamReadBitsIntoList(readerFromStartBeforeLoop, 1)
            val srcListFromI = byteArrayBitContentToList(srcBuffer, i, 1)
            check(srcListFromI.head == listOfBitsFromStartBeforeLoop.head)
         })

         appendBitsMSBFirstLoop(srcBuffer, i + 1, to)

         ghostExpr(BitStream.lemmaIsPrefixTransitive(beforeAppend, afterAppend, this))
         assert(afterAppend.isPrefixOf(this))
         assert(beforeAppend.isPrefixOf(this))

         ghostExpr({
            val snapThis = snapshot(this)
            assert(BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(beforeAppend.buf.length, beforeAppend.currentByte, beforeAppend.currentBit ) + to - i)
            assert(BitStream.invariant(currentBit, currentByte, buf.length) )
            assert(beforeAppend.buf.length == this.buf.length)
            assert(beforeAppend.isPrefixOf(this) )
            check(buf.length == beforeAppend.buf.length)
            check(BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(beforeAppend.buf.length, beforeAppend.currentByte, beforeAppend.currentBit ) + to - i)
            check(BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(afterAppend.buf.length, afterAppend.currentByte, afterAppend.currentBit ) + to - i - 1)
            assert(afterAppend.buf.length == this.buf.length)
            assert(BitStream.invariant(afterAppend.currentBit, afterAppend.currentByte, afterAppend.buf.length) )
            assert(BitStream.invariant(afterAppend.currentBit, afterAppend.currentByte, this.buf.length) )

            val (readerFromStart, _) = reader(beforeAppend, this)
            validateOffsetBitsContentIrrelevancyLemma(beforeAppend, this.buf, to - i)

            val (readerFromAfterAppend, _) = reader(afterAppend, this)

            validateOffsetBitsContentIrrelevancyLemma(afterAppend, this.buf, to - i - 1)

            val srcListFromI = byteArrayBitContentToList(srcBuffer, i, to - i)
            val srcListFromIPlusOne = byteArrayBitContentToList(srcBuffer, i + 1, to - i - 1)

            val listOfBitsFromStart = bitStreamReadBitsIntoList(readerFromStart, to - i)
            val listOfBitsFromAfterAppend = bitStreamReadBitsIntoList(readerFromAfterAppend,  to - i - 1)

            assert(to - i != 0)
            assert(listOfBitsFromStart.length > 0)
            lemmaBitStreamReadBitsIntoListFromBitIndexPlusOneIsTail(snapThis, readerFromStart, readerFromAfterAppend, to - i, listOfBitsFromStart)
            assert(listOfBitsFromStart.tail == listOfBitsFromAfterAppend)

            assert(bitAt(readerFromStart.buf, bitIndexBeforeAppend) == bitAt(readerFromAfterAppend.buf, bitIndexBeforeAppend))
            assert(listOfBitsFromStart.head == bitAt(readerFromStart.buf, bitIndexBeforeAppend))
            assert(srcListFromI.head == bitAt(srcBuffer.toArrayRaws, i))
            assert(bitAt(afterAppend.buf, bitIndexBeforeAppend) == bitAt(srcBuffer.toArrayRaws, i))

            assert(afterAppend.isPrefixOf(this))
            assert(BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(afterAppend.buf.length, afterAppend.currentByte, afterAppend.currentBit ) + to - i - 1)
            arrayBitRangesEqImpliesEq(afterAppend.buf, this.buf, 0, bitIndexBeforeAppend, BitStream.bitIndex(afterAppend.buf.length, afterAppend.currentByte, afterAppend.currentBit))
            assert(bitAt(afterAppend.buf, bitIndexBeforeAppend) == bitAt(this.buf, bitIndexBeforeAppend))

            assert(bitAt(this.buf, bitIndexBeforeAppend) == bitAt(srcBuffer.toArrayRaws, i))
            assert(readerFromStart.readBitPure()._2 == bitAt(srcBuffer.toArrayRaws, i))

            assert(listOfBitsFromStart.head == srcListFromI.head)

         })
      else
         ghostExpr({
            assert(BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(beforeAppend.buf.length, beforeAppend.currentByte, beforeAppend.currentBit ) + to - i)
            assert(BitStream.invariant(currentBit, currentByte, buf.length) )
            assert(beforeAppend.buf.length == this.buf.length )
            assert(beforeAppend.isPrefixOf(this) )
            check(buf.length == beforeAppend.buf.length)
            val (r1, r2) = reader(beforeAppend, this)
            val vGot = r1.readBits(to - i)
            assert(to - i == 0)
            check(byteArrayBitContentSame(srcBuffer, vGot, i, 0, to - i))
         })

   }.ensuring( _ =>
         BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit ) + to - i
         &&& BitStream.invariant(currentBit, currentByte, buf.length)
         &&& old(this).buf.length == this.buf.length
         &&& old(this).isPrefixOf(this)
         &&&
         (
            buf.length == old(this).buf.length
            &&
            {
               val (r1, r2) = reader(old(this), this)
               validateOffsetBitsContentIrrelevancyLemma(old(this), this.buf, to - i)
               val listBits = bitStreamReadBitsIntoList(r1, to - i)
               val srcList = byteArrayBitContentToList(srcBuffer, i, to - i)
               listBits == srcList
            }
         )
      )
   /**
    * Append nBits from srcBuffer to bitstream
    *
    * @param srcBuffer source of the bits to add
    * @param nBits number of bits to add
    * @param from start index in srcBuffer (in bits index, not UByte!!)
    *
    * Remarks:
    * bit 0 is the MSB of the first byte of srcBuffer
    *
    */
   def appendBitsMSBFirst(srcBuffer: Array[UByte], nBits: Long, from: Long = 0): Unit = {
      require(nBits >= 0)
      require(from >= 0)
      require(from < Long.MaxValue - nBits)
      require(nBits + from <= srcBuffer.length.toLong * 8L)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))

      @ghost val oldThis = snapshot(this)
      @ghost val oldSrc = snapshot(srcBuffer)
      appendBitsMSBFirstLoop(srcBuffer, from, from + nBits)
      ghostExpr({
         val w1 = oldThis
         val w2 = this
         assert(srcBuffer == oldSrc)
         assert(BitStream.invariant(currentBit, currentByte, buf.length) )
         assert(w1.buf.length == w2.buf.length )
         assert(BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + nBits)
         assert(w1.isPrefixOf(w2) )


         val (r1, _) = reader(w1, w2)
         validateOffsetBitsContentIrrelevancyLemma(w1, w2.buf, nBits)
         val vGot = r1.readBits(nBits)
         val (readerr, _) = reader(w1, w2)
         // lemmaReadBitsThenGetListIsSameAsGetList(vGot, readerr, nBits)
         assert(byteArrayBitContentToList(vGot, 0, nBits) == bitStreamReadBitsIntoList(readerr, nBits))

         val (readerrr, _) = reader(w1, w2)
         assert(bitStreamReadBitsIntoList(readerrr, nBits) == byteArrayBitContentToList(srcBuffer, from, nBits)) // Should work

         lemmaSameBitContentListThenbyteArrayBitContentSame(srcBuffer, vGot, from, 0, nBits)
         assert(byteArrayBitContentSame(srcBuffer, vGot, from, 0, nBits) )
      })

   }.ensuring(_ =>
      val w1 = old(this)
      val w2 = this
      srcBuffer == old(srcBuffer)
      &&& BitStream.invariant(currentBit, currentByte, buf.length)
      &&& w1.buf.length == w2.buf.length
      &&& BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit) ==
            BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + nBits
      &&& w1.isPrefixOf(w2)
      &&&
      {
         val (r1, r2) = reader(w1, w2)
         validateOffsetBitsContentIrrelevancyLemma(w1, w2.buf, nBits)
         val vGot = r1.readBits(nBits)

         byteArrayBitContentSame(srcBuffer, vGot, from, 0, nBits)
      }
   )

    /**
     * Transforms an array of UByte, for example resulting from a readBits call, into a list of bits, for specification purposes
     *
     */
   @ghost
   @pure
   def bitStreamReadBitsIntoList(bitStream: BitStream, nBits: Long): List[Boolean] = {
      require(nBits >= 0)
      require(BitStream.validate_offset_bits(bitStream.buf.length.toLong, bitStream.currentByte.toLong, bitStream.currentBit.toLong, nBits))

      decreases(nBits)
      val bitStreamSnap = snapshot(bitStream)
      if(nBits == 0) then
         Nil()
      else
         val bit = bitStreamSnap.readBit()
         Cons(bit, bitStreamReadBitsIntoList(bitStreamSnap, nBits - 1))
   }.ensuring( res => if(nBits == 0) then res.isEmpty else res.length > 0 ) // we'd like to prove res.length == nBits but it's not possible because of type mismatch

   @ghost
   @opaque
   @inlineOnce
   def lemmaBitStreamReadBitsIntoListFromBitIndexPlusOneIsTail(base: BitStream, bitStream1: BitStream, bitStream2: BitStream, nBits: Long, listBits: List[Boolean]): Unit = {
      require(nBits > 0)
      require(listBits.length > 0)
      require(bitStream1.isPrefixOf(base))
      require(bitStream2.isPrefixOf(base))
      require(bitStream1.isPrefixOf(bitStream2))
      require(bitStream1.buf == bitStream2.buf)
      require(bitStream1.buf == base.buf)
      require(BitStream.bitIndex(base.buf.length, base.currentByte, base.currentBit) < Long.MaxValue - nBits)
      require(BitStream.bitIndex(bitStream1.buf.length, bitStream1.currentByte, bitStream1.currentBit) + 1 == BitStream.bitIndex(bitStream2.buf.length, bitStream2.currentByte, bitStream2.currentBit))
      require(BitStream.validate_offset_bits(bitStream1.buf.length.toLong, bitStream1.currentByte.toLong, bitStream1.currentBit.toLong, nBits))
      require(BitStream.validate_offset_bits(bitStream2.buf.length.toLong, bitStream2.currentByte.toLong, bitStream2.currentBit.toLong, nBits - 1))
      require(listBits == bitStreamReadBitsIntoList(bitStream1, nBits))

      if nBits == 1 then
         ()
      else
         val bitStream1Snap = snapshot(bitStream1)
         assert(bitStream1.readBitPure()._2 == listBits.head)
         ()
   }.ensuring(_ =>
      bitStreamReadBitsIntoList(bitStream2, nBits - 1) == listBits.tail
   )


   /**
     * Transforms an array of UByte, for example resulting from a readBits call, into a list of bits, for specification purposes
     *
     * @param arr
     * @param from
     * @param nBits
     */
   @ghost
   @pure
   def byteArrayBitContentToList(arr: Array[UByte], from: Long, nBits: Long): List[Boolean] = {
      require(from >= 0)
      require(nBits >= 0)
      require(from < Long.MaxValue - nBits)
      require(from + nBits <= arr.length.toLong * 8L)
      decreases(nBits)
      if(nBits == 0) then
         Nil()
      else
         val byteIndex = from / 8
         val bitIndex = from % 8
         val mask = BitAccessMasks(bitIndex.toInt)
         val b = (arr(byteIndex.toInt).toRaw & mask) != 0
         Cons(b, byteArrayBitContentToList(arr, from + 1, nBits - 1))
   }


   /**
     * Compare the content of two byte arrays at the bit level, from bit from to bit to (from is included, to is excluded)
     *
     * @param arr1
     * @param arr2
     * @param from1
     * @param from2
     * @param nBits
     */
   @ghost
   @pure
   def byteArrayBitContentSame(arr1: Array[UByte], arr2: Array[UByte], from1: Long, from2: Long, nBits: Long): Boolean = {
      require(from1 >= 0)
      require(from2 >= 0)
      require(nBits >= 0)
      require(from2 < Long.MaxValue - nBits)
      require(from1 < Long.MaxValue - nBits)
      require(from1 + nBits <= arr1.length.toLong * 8L)
      require(from2 + nBits <= arr2.length.toLong * 8L)
      decreases(nBits)
      if(nBits == 0) then
         true
      else
         val byteIndex1 = from1 / 8
         val bitIndex1 = from1 % 8
         val byteIndex2 = from2 / 8
         val bitIndex2 = from2 % 8
         val mask1 = BitAccessMasks(bitIndex1.toInt)
         val mask2 = BitAccessMasks(bitIndex2.toInt)
         val b1 = (arr1(byteIndex1.toInt).toRaw & mask1) != 0
         val b2 = (arr2(byteIndex2.toInt).toRaw & mask2) != 0
         if b1 != b2 then
            false
         else
            byteArrayBitContentSame(arr1, arr2, from1 + 1, from2 + 1, nBits - 1)
   }

   @opaque
   @ghost
   @pure
   def lemmaSameBitContentListThenbyteArrayBitContentSame(arr1: Array[UByte], arr2: Array[UByte], fromArr1: Long, fromArr2: Long, nBits: Long): Unit = {
      require(fromArr1 >= 0)
      require(fromArr2 >= 0)
      require(nBits >= 0)
      require(fromArr1 < Long.MaxValue - nBits)
      require(fromArr2 < Long.MaxValue - nBits)
      require(fromArr1 + nBits <= arr1.length.toLong * 8L)
      require(fromArr2 + nBits <= arr2.length.toLong * 8L)
      require(byteArrayBitContentToList(arr2, fromArr2, nBits) == byteArrayBitContentToList(arr1, fromArr1, nBits))
      decreases(nBits)

      if nBits > 0 then
         lemmaSameBitContentListThenbyteArrayBitContentSame(arr1, arr2, fromArr1 + 1, fromArr2 + 1, nBits - 1)
   }.ensuring(_ => byteArrayBitContentSame(arr1, arr2, fromArr1, fromArr2, nBits))



   @extern
   def appendBitsMSBFirstVec(srcBuffer: Vector[UByte], nBits: Long, from: Long = 0): Unit = {
      require(nBits >= 0)
      require(from >= 0)
      require(from < Long.MaxValue - nBits)
      require(nBits + from <= srcBuffer.length.toLong * 8L)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))
      appendBitsMSBFirst(srcBuffer.toScala.toArray, nBits, from)
   }.ensuring(_ => srcBuffer == old(srcBuffer) && buf.length == old(this).buf.length && BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit ) + nBits)

   // ****************** Append Byte Functions **********************

   /**
    * Append part of a byte to the bitstream
    *
    * @param v value that should be partially added
    * @param nBits that should get taken from v - counting starts with the LSB
    *
    * Example:
    *
    * nBits =  3
    *          MSB = 2^7            LSB = 2^0
    *          |                    |
    * v =      1  0  0  1  0  1  1  0
    *                         |  |  |
    *                         b1 b2 b3
    *
    * b1 to b3 get added to the bitstream - starting with b1
    *
    */

   @opaque @inlineOnce
   def appendPartialByte(v: UByte, nBits: Int): Unit = {
      require(nBits >= 1 && nBits < NO_OF_BITS_IN_BYTE)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))

      @ghost val oldThis = snapshot(this)

      val cb = currentBit
      val totalBits = cb + nBits
      val ncb = 8 - cb

      val mask1 = (~MASK_B(ncb)).toByte
      val vv = (v.toRaw & MASK_B(nBits)).toByte
      if totalBits <= 8 then
         val mask2 = MASK_B(8 - totalBits)
         val mask = (mask1 | mask2).toByte
         buf(currentByte) = wrappingExpr { ((buf(currentByte) & mask) | (vv << (8 - totalBits))).toByte }
         ghostExpr {
            arrayUpdatedAtPrefixLemma(oldThis.buf, currentByte, buf(currentByte))
            assert(arrayRangesEq(oldThis.buf, buf, 0, currentByte))
            assert(
               byteRangesEq(
                  oldThis.buf(oldThis.currentByte),
                  buf(oldThis.currentByte),
                  0,
                  oldThis.currentBit
               )
            )
         }
         moveBitIndex(nBits)
      else
         val totalBitsForNextByte = totalBits - 8
         buf(currentByte) = wrappingExpr { ((buf(currentByte) & mask1) | ((vv & 0XFF) >>> totalBitsForNextByte)).toByte }
         @ghost val oldThis2 = snapshot(this)
         currentByte += 1
         val mask = MASK_B(8 - totalBitsForNextByte).toByte
         buf(currentByte) = wrappingExpr { ((buf(currentByte) & mask) | (vv << (8 - totalBitsForNextByte))).toByte }
         ghostExpr {
            arrayUpdatedAtPrefixLemma(oldThis.buf, currentByte - 1, buf(currentByte - 1))
            arrayUpdatedAtPrefixLemma(oldThis2.buf, currentByte, buf(currentByte))
            arrayRangesEqTransitive(
               oldThis.buf,
               oldThis2.buf,
               buf,
               0, currentByte - 1, currentByte
            )
            assert(
               byteRangesEq(
                  oldThis.buf(oldThis.currentByte),
                  buf(oldThis.currentByte),
                  0,
                  totalBitsForNextByte
               )
            )
         }
         currentBit = totalBitsForNextByte
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      w1.buf.length == w2.buf.length && BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + nBits
      && w1.isPrefixOf(w2) && {
         val (r1, r2) = reader(w1, w2)
         val (r2Got, vGot) = r1.readPartialBytePure(nBits)
         vGot.toRaw == wrappingExpr { (v.toRaw & MASK_B(nBits)).toByte } && r2Got == r2
      }
  }

   /**
    * Append whole byte to bitstream
    *
    * @param v gets appended to the bitstream
    *
    * Remarks:
    * The MSB is written first into the bitstream
    *
    * Example:
    * cur bit on Bitstream = 3
    *
    *       MSB           LSB
    *        |             |
    *  x x x b b b b b b b b
    *  _ _ _ _ _ _ _ _ _ _ _ _ _
    *  0 1 2 3 4 5 6 7 0 1 2 3 4 ...
    *
    * Pos 3 (MSB of v) to 10 (LSB of v) are written
    *
    * */
   @opaque @inlineOnce
   def appendByte(v: UByte): Unit = {
      require(BitStream.validate_offset_byte(buf.length.toLong, currentByte.toLong, currentBit.toLong))

      @ghost val oldThis = snapshot(this)
      val cb = currentBit.toByte
      val ncb = (8 - cb).toByte
      var mask = (~MASK_B(ncb)).toByte

      buf(currentByte) = wrappingExpr { (buf(currentByte) & mask).toByte }
      buf(currentByte) = wrappingExpr { (buf(currentByte) | ((v.toRaw & 0xFF) >>> cb)).toByte }
      currentByte += 1

      ghostExpr {
         check(
         (oldThis.currentByte < oldThis.buf.length) ==>
            byteRangesEq(
               oldThis.buf(oldThis.currentByte),
               buf(oldThis.currentByte),
               0, oldThis.currentBit))
      }
      @ghost val oldThis2 = snapshot(this)

      if cb > 0 then
         mask = (~mask).toByte
         buf(currentByte) = wrappingExpr { (buf(currentByte) & mask).toByte }
         buf(currentByte) = wrappingExpr { (buf(currentByte) | (v.toRaw << ncb)).toByte }

      ghostExpr {
         arrayUpdatedAtPrefixLemma(oldThis.buf, currentByte - 1, buf(currentByte - 1))
         assert(arrayRangesEq(oldThis.buf, oldThis2.buf, 0, currentByte - 1))

         if cb > 0 then
            arrayUpdatedAtPrefixLemma(oldThis.buf, currentByte, buf(currentByte))
            arrayUpdatedAtPrefixLemma(oldThis2.buf, currentByte, buf(currentByte))
            arrayRangesEqTransitive(
               oldThis.buf,
               oldThis2.buf,
               buf,
               0, currentByte - 1, currentByte
            )
            check(arrayRangesEq(
               oldThis.buf,
               buf,
               0,
               oldThis.currentByte
            ))
         else
            check(arrayRangesEq(
               oldThis.buf,
               buf,
               0,
               oldThis.currentByte
            ))
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      w1.buf.length == w2.buf.length && BitStream.bitIndex(w2.buf.length, w2.currentByte, w2.currentBit) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + 8
      && w1.isPrefixOf(w2)
      && {
         val (r1, r2) = reader(w1, w2)
         val (r2Got, vGot) = r1.readBytePure()
         vGot == v && r2Got == r2
      }
   }

   /**
    * NBytes of the given array is added to the bitstream
    *
    * @param arr is the source array
    * @param noOfBytes that get written into the bitstream
    *
    * Remarks:
    * The MSB of the arr[0] is written first
    *
    */
   def appendByteArray(arr: Array[UByte], noOfBytes: Int): Unit = {
      require(0 <= noOfBytes && noOfBytes <= arr.length)
      require(BitStream.validate_offset_bytes(buf.length.toLong, currentByte.toLong, currentBit.toLong, noOfBytes))

      appendByteArrayLoop(arr, 0, noOfBytes)
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.buf.length == w3.buf.length
      && BitStream.bitIndex(w3.buf.length, w3.currentByte, w3.currentBit) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + noOfBytes.toLong * 8L
      && w1.isPrefixOf(w3)
      && {
         val (r1, r3) = reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1, w3.buf, noOfBytes)
         val (r3Got, arrGot) = r1.readByteArrayLoopPure(arr, 0, noOfBytes)
         arrGot.length == arr.length && r3Got == r3 && arrayRangesEq(arr, arrGot, 0, noOfBytes)
      }
   }

   @opaque @inlineOnce
   def appendByteArrayLoop(arr: Array[UByte], from: Int, to: Int): Unit = {
      require(0 <= from && from <= to && to <= arr.length)
      require(BitStream.validate_offset_bytes(buf.length.toLong, currentByte.toLong, currentBit.toLong, to - from))
      decreases(to - from)
      if (from < to) {
         @ghost val oldThis1 = snapshot(this)
         assert(oldThis1.buf.length.toLong == buf.length.toLong)
         assert(oldThis1.currentByte.toLong == currentByte.toLong)
         assert(oldThis1.currentBit.toLong == currentBit.toLong)
         assert(BitStream.invariant( oldThis1.currentBit, oldThis1.currentByte, oldThis1.buf.length))
         assert((BitStream.validate_offset_bytes(oldThis1.buf.length.toLong, oldThis1.currentByte.toLong, oldThis1.currentBit.toLong, to - from)))
         appendByte(arr(from))
         @ghost val oldThis2 = snapshot(this)
         ghostExpr {
            assert((BitStream.validate_offset_bytes(oldThis1.buf.length.toLong, oldThis1.currentByte.toLong, oldThis1.currentBit.toLong, to - from)))
            validateOffsetBytesFromBitIndexLemma(oldThis1, this, 8, to - from)
         }
         appendByteArrayLoop(arr, from + 1, to)

         ghostExpr {
            lemmaIsPrefixTransitive(oldThis1, oldThis2, this)
            val oldThis2Reset = oldThis2.resetAt(oldThis1)
            readBytePrefixLemma(oldThis2Reset, this)
            val (r1_13, r3_13) = reader(oldThis1, this)
            val (r2_23, r3_23) = reader(oldThis2, this)
            val (_, byteGot) = r1_13.readBytePure()
            check(byteGot == arr(from))
            validateOffsetBytesContentIrrelevancyLemma(oldThis1, this.buf, to - from)
            val (r3Got_13, arrGot_13) = r1_13.readByteArrayLoopPure(arr, from, to)
            check(r3Got_13 == r3_13)
            validateOffsetBytesContentIrrelevancyLemma(oldThis2, this.buf, to - from - 1)
            val (r3Got_23, arrGot_23) = r2_23.readByteArrayLoopPure(arr.updated(from, byteGot), from + 1, to)
            readByteArrayLoopArrayPrefixLemma(r1_13, arr, from, to)
            assert(arrayRangesEq(arrGot_13, arrGot_23, 0, to))
            arrayRangesEqSymmetricLemma(arrGot_13, arrGot_23, 0, to)
            arrayRangesEqTransitive(arr, arrGot_23, arrGot_13, 0, to, to)
            check(arrayRangesEq(arr, arrGot_13, 0, to))
         }
      } else {
         ghostExpr {
            lemmaIsPrefixRefl(this)
         }
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.buf.length == w3.buf.length
      && BitStream.bitIndex(w3.buf.length, w3.currentByte, w3.currentBit) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + (to - from).toLong * 8L
      && w1.isPrefixOf(w3)
      && {
         val (r1, r3) = reader(w1, w3)
         validateOffsetBitsContentIrrelevancyLemma(w1, w3.buf, to - from)
         val (r3Got, arrGot) = r1.readByteArrayLoopPure(arr, from, to)
         arrGot.length == arr.length && r3Got == r3 && arrayRangesEq(arr, arrGot, 0, to)
      }
   }

   @extern
   def appendByteVec(arr: Vector[UByte], noOfBytes: Int): Unit = {
      require(0 <= noOfBytes && noOfBytes <= arr.length)
      require(BitStream.validate_offset_bytes(buf.length.toLong, currentByte.toLong, currentBit.toLong, noOfBytes))

      appendByteArray(arr.toScala.toArray, noOfBytes)
   }.ensuring { _ =>
      val w1 = old(this)
      val w3 = this
      w1.buf.length == w3.buf.length && BitStream.bitIndex(w3.buf.length, w3.currentByte, w3.currentBit) == BitStream.bitIndex(w1.buf.length, w1.currentByte, w1.currentBit) + noOfBytes.toLong * 8L
   }

   // ****************** Peak Functions **********************

   /**
    * Preview the next bit on the bitstream
    *
    * @return peeked bit
    *
    */
   @pure
   def peekBit(): Boolean = {
      require(BitStream.validate_offset_bit(buf.length.toLong, currentByte.toLong, currentBit.toLong))
      ((buf(currentByte) & 0xFF) & (BitAccessMasks(currentBit) & 0xFF)) > 0
   }

   // ****************** Read Bit Functions **********************

   /**
    * Read single bit from the bitstream
    *
    * @return next bit on the bitstream
    *
    */
   def readBit(): Boolean = {
      require(validate_offset_bits(1))
      val ret = (buf(currentByte) & BitAccessMasks(currentBit)) != 0
      increaseBitIndex()
      ret
   }.ensuring(_ => buf == old(this).buf && BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit ) + 1)

   @ghost @pure
   def readBitPure(): (BitStream, Boolean) = {
      require(validate_offset_bits(1))
      val cpy = snapshot(this)
      val b = cpy.readBit()
      (cpy, b)
   }

   /**
    * Read multiple bits from the bitstream
    *
    * @param nBits number of bits to read
    * @return array of read bits
    *
    * Remarks:
    * First bit is written into the MSB of Byte 0
    *
    * Example:
    * nBits = 10
    * curBits on bitstream = 3
    *
    * bits on stream  x  x  b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 x
    * currentBit      0  1  2  3  4  5  6  7  0  1  2  3  4 ...
    *                 |                       |
    *            start of byte n        start of byte n+1
    *
    * arr = ByteArray with size 2 (10bits need 2 bytes) get
    * returned with this structure
    *                       LSB byte 0             LSB byte 1
    *                           |           | 0 padding |
    *    b0 b1 b2 b3 | b4 b5 b6 b7 || b8 b9 0 0 | 0 0 0 0
    * i: 1  2  3  4    5  6  7  8     9  10
    *    |                            |
    *  MSB byte 0                 MSB byte 1
    *
    */
   def readBits(nBits: Long): Array[UByte] = {
      require(0 <= nBits && nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))

      val arrLen = ((nBits + NO_OF_BITS_IN_BYTE - 1) / NO_OF_BITS_IN_BYTE).toInt
      val arr: Array[Byte] = Array.fill(arrLen)(0 : Byte)
      readBitsLoop(nBits, arr, 0, nBits)
      UByte.fromArrayRaws(arr)
   }.ensuring(res =>
      buf == old(this).buf
      &&& BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit) + nBits == BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit)
      &&& BitStream.invariant(this.currentBit, this.currentByte, this.buf.length)
      &&& res.length == ((nBits + NO_OF_BITS_IN_BYTE - 1) / NO_OF_BITS_IN_BYTE).toInt
      &&& old(this).currentByte <= this.currentByte
      &&& byteArrayBitContentToList(res, 0, nBits) == bitStreamReadBitsIntoList(old(this), nBits)
      )


   def readBitsLoop(nBits: Long, arr: Array[Byte], from: Long, to: Long): Unit = {
      require(0 <= nBits && nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      require(arr.length >= ((nBits + NO_OF_BITS_IN_BYTE - 1) / NO_OF_BITS_IN_BYTE))
      require(0 <= from && from <= to && to <= nBits)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits - from))
      decreases(to - from)
      if (from < to) {
         @ghost val arr1 = snapshot(arr)
         @ghost val oldThis1 = snapshot(this)

         val bit = readBit()
         val byteIx = (from / NO_OF_BITS_IN_BYTE).toInt
         val bitIx = (from % NO_OF_BITS_IN_BYTE).toInt

         arr(byteIx) = stainless.math.wrapping { ((arr(byteIx) & ~BitAccessMasks(bitIx)) | (if bit then BitAccessMasks(bitIx) else 0)).toByte }
         @ghost val arr2 = snapshot(arr)
         @ghost val oldThis2 = snapshot(this)
         ghostExpr {
            BitStream.validateOffsetBitsIneqLemma(oldThis1, this, nBits - from, 1)
         }
         readBitsLoop(nBits, arr, from + 1, to)

         ghostExpr {
            check {
               BitStream.bitIndex(oldThis1.buf.length, oldThis1.currentByte, oldThis1.currentBit ) + to - from == BitStream.bitIndex(buf.length, currentByte, currentBit) &&
               oldThis1.buf == buf && arr1.length == arr.length
            }

            arrayBitRangesUpdatedAtLemma(arr1, from, bit)
            arrayBitRangesEqTransitive(arr1, arr2, arr, 0, from, from + 1)
            check(arrayBitRangesEq(arr1, arr, 0, from))

            arrayBitRangesEqImpliesEq(arr2, arr, 0, from, from + 1)
            check(arrayBitRangesEq(arr1, arr, 0, from))
            check(bitAt(arr, from) == bit)
         }
      } else {
         ghostExpr {
            arrayBitRangesEqReflexiveLemma(arr)
            arrayBitRangesEqSlicedLemma(arr, snapshot(arr), 0, arr.length.toLong * 8L, 0, from)
         }
      }
   }.ensuring { _ =>
      BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit ) + to - from == BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit ) &&
      old(this).buf == this.buf
      &&& old(arr).length == arr.length
      &&& arrayBitRangesEq(old(arr), arr, 0, from)
      &&& (if (from < to) then (bitAt(arr, from) == old(this).readBitPure()._2) else true)
      &&& BitStream.invariant(this.currentBit, this.currentByte, this.buf.length)
      &&& byteArrayBitContentToList(UByte.fromArrayRaws(arr), from, to - from) == bitStreamReadBitsIntoList(old(this), to - from)
   }

   @extern
   def readBitsVec(nBits: Long): Vector[UByte] = {
      require(0 <= nBits && nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))
      val arr = readBits(nBits)
      Vector.fromScala(arr.toVector)
   }.ensuring(res =>
      buf == old(this).buf && BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit) + nBits == BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit) &&
      BitStream.invariant(this.currentBit, this.currentByte, this.buf.length) &&
      res.length == ((nBits + NO_OF_BITS_IN_BYTE - 1) / NO_OF_BITS_IN_BYTE).toInt
   )

   @pure @ghost
   def readBitsVecPure(nBits: Long): (BitStream, Vector[UByte]) = {
      require(0 <= nBits && nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))
      val cpy = snapshot(this)
      val res = cpy.readBitsVec(nBits)
      (cpy, res)
   }

   def checkBitsLoop(nBits: Long, expected: Boolean, from: Long): Boolean = {
      require(0 <= nBits && nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      require(0 <= from && from <= nBits)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits - from))
      decreases(nBits - from)
      if (from == nBits) true
      else {
         @ghost val oldThis = snapshot(this)
         val bit = readBit()
         if (bit != expected) false
         else {
            ghostExpr {
               BitStream.validateOffsetBitsIneqLemma(oldThis, this, nBits - from, 1)
            }
            checkBitsLoop(nBits, expected, from + 1)
         }
      }
   }.ensuring { ok =>
      BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit ) + nBits - from >= BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit ) &&
      old(this).buf == this.buf &&
      (if(nBits == from) ok else true) &&
      (ok ==> (BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit ) + nBits - from == BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit )))
      && ((ok && from < nBits) ==> (expected == old(this).readBitPure()._2))
   }

   @ghost @pure
   def checkBitsLoopPure(nBits: Long, expected: Boolean, from: Long): (BitStream, Boolean) = {
      require(0 <= nBits && nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      require(0 <= from && from <= nBits)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits - from))
      val cpy = snapshot(this)
      val res = cpy.checkBitsLoop(nBits, expected, from)
      (cpy, res)
   }

   /**
    * Counter Operation to appendBitsLSBFirst
    * @param nBits number of bits to read [0-64]
    * @return value that holds nBits from bitstream
    *
    * Remarks:
    * The first bit from the bitstream will get written into the LSB
    */
   def readNBitsLSBFirst(nBits: Int): Long = {
      require(nBits >= 0 && nBits <= 64)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))
      readNBitsLSBFirstsLoop(nBits, 0, 0L)
   }.ensuring(_ =>
         buf == old(this).buf
         && BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit) + nBits)

   @ghost @pure
   def readNBitsLSBFirstPure(nBits: Int): (BitStream, Long) = {
      require(nBits >= 0 && nBits <= 64)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))
      val cpy = snapshot(this)
      val res = cpy.readNBitsLSBFirst(nBits)
      (cpy, res)
   }

   def readNBitsLSBFirstsLoop(nBits: Int, i: Int, acc: Long): Long = {
      require(0 <= i && i <= nBits && nBits <= 64)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits - i))
      require((acc & onesMSBLong(64 - i)) == 0L) //  The 64 - i MSBs must be 0
      require((acc & onesMSBLong(64)) == acc)
      decreases(nBits - i)
      if (nBits == i) {
         acc
      } else {
         val bit = readBit()
         val newAcc = acc | (if bit then 1L << i else 0)
         readNBitsLSBFirstsLoop(nBits, i + 1, newAcc)
      }
   }.ensuring { res =>
      buf == old(this).buf &&
      BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit ) + (nBits - i)
      && (res & onesLSBLong(i)) == (acc & onesLSBLong(i))
      && (res & onesLSBLong(nBits)) == res
      && (if (i < nBits) then ((((res >>>  i) & 1) == 1) == old(this).readBitPure()._2) else true)
   }

   @ghost @pure
   def readNBitsLSBFirstsLoopPure(nBits: Int, i: Int, acc: Long): (BitStream, Long) = {
      require(0 <= i && i <= nBits && nBits <= 64)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits - i))
      require((acc & onesMSBLong(64 - i)) == 0L) //  The 64 - i MSBs must be 0
      require((acc & onesMSBLong(64)) == acc)
      val cpy = snapshot(this)
      val res = cpy.readNBitsLSBFirstsLoop(nBits, i, acc)
      (cpy, res)
   }

   /**
    * Counter Operation to appendLSBBitsMSBFirst
    * @param nBits number of bits to read [0-64]
    * @return value that holds nBits from bitstream
    *
    * Remarks:
    * The last bit from the bitstream will get written into the LSB
    */
   def readNLSBBitsMSBFirst(nBits: Int): Long = {
      require(nBits >= 0 && nBits <= 64)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))
      readNLSBBitsMSBFirstLoop(nBits, 0, 0L)
   }.ensuring(_ => buf == old(this).buf && BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit) + nBits)

   @ghost @pure
   def readNLSBBitsMSBFirstPure(nBits: Int): (BitStream, Long) = {
      require(nBits >= 0 && nBits <= 64)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))
      val cpy = snapshot(this)
      val res = cpy.readNLSBBitsMSBFirst(nBits)
      (cpy, res)
   }

   def readNLSBBitsMSBFirstLoop(nBits: Int, i: Int, acc: Long): Long = {
      require(0 <= i && i <= nBits && nBits <= 64)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits - i))
      require((acc & onesLSBLong(nBits - i)) == 0L) //  The nBits - i LSBs must be 0
      require((acc & onesLSBLong(nBits)) == acc)
      decreases(nBits - i)
      if (nBits == i) {
         acc
      } else {
         val bit = readBit()
         val newAcc = acc | (if bit then 1L << (nBits - 1 - i) else 0)
         readNLSBBitsMSBFirstLoop(nBits, i + 1, newAcc)
      }
   }.ensuring { res =>
      buf == old(this).buf &&
      BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit ) + (nBits - i) &&
      (res >>> (nBits - i) == acc >>> (nBits - i)) &&
      (res & onesLSBLong(nBits)) == res &&
      (i < nBits) ==> ((((res >>> (nBits - 1 - i)) & 1) == 1) == old(this).readBitPure()._2)
   }

   @ghost @pure
   def readNLSBBitsMSBFirstLoopPure(nBits: Int, i: Int, acc: Long): (BitStream, Long) = {
      require(0 <= i && i <= nBits && nBits <= 64)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits - i))
      require((acc & onesLSBLong(nBits - i)) == 0L) //  The nBits - i LSBs must be 0
      require((acc & onesLSBLong(nBits)) == acc)
      val cpy = snapshot(this)
      val res = cpy.readNLSBBitsMSBFirstLoop(nBits, i, acc)
      (cpy, res)
   }

   // ****************** Read Byte Functions **********************

   /**
    * Read whole byte from the bitstream
    *
    * @return byte read from bitstream
    *
    * Remarks:
    * First bit read from bitstream is the return bytes MSB
    *
    */
   def readByte(): UByte = {
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, 8))

      val cb = this.currentBit.toByte
      val ncb = (8 - cb).toByte
      var v = wrappingExpr { (this.buf(this.currentByte) << cb).toByte }
      this.currentByte += 1

      if cb > 0 then
         v = wrappingExpr { (v | (this.buf(this.currentByte) & 0xFF) >>> ncb).toByte }
      UByte.fromRaw(v)
   }.ensuring(_ =>
      buf == old(this).buf
      && BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit ) + 8
      )

   @ghost @pure
   def readBytePure(): (BitStream, UByte) = {
      require(BitStream.validate_offset_byte(buf.length.toLong, currentByte.toLong, currentBit.toLong))
      val cpy = snapshot(this)
      val res = cpy.readByte()
      (cpy, res)
   }

   def readByteArray(nBytes: Int): Array[UByte] = {
      require(nBytes >= 0)
      require(BitStream.validate_offset_bytes(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBytes))

      val arr = Array.fill(nBytes)(UByte.fromRaw(0))
      readByteArrayLoop(arr, 0, nBytes)
      arr
   }

   def readByteArrayLoop(arr: Array[UByte], i: Int, to: Int): Unit = {
      require(0 <= i && i <= to && to <= arr.length)
      require(BitStream.validate_offset_bytes(buf.length.toLong, currentByte.toLong, currentBit.toLong, to - i))
      decreases(to - i)

      @ghost val oldThis1 =snapshot(this)
      if (i < to) {
         @ghost val arr1 = snapshot(arr)
         val b = readByte()
         arr(i) = b

         @ghost val arr2 = snapshot(arr)
         @ghost val oldThis2 = snapshot(this)
         ghostExpr {
            validateOffsetBytesFromBitIndexLemma(oldThis1, this, 8, to - i)
         }
         readByteArrayLoop(arr, i + 1, to)

         ghostExpr {
            check(BitStream.bitIndex(oldThis2.buf.length, oldThis2.currentByte, oldThis2.currentBit ) == BitStream.bitIndex(oldThis1.buf.length, oldThis1.currentByte, oldThis1.currentBit) + 8L)
            check(BitStream.bitIndex(oldThis2.buf.length, oldThis2.currentByte, oldThis2.currentBit ) + (to - i - 1) * 8L == BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit))
            check(BitStream.bitIndex(oldThis1.buf.length, oldThis1.currentByte, oldThis1.currentBit ) + (to - i) * 8L == BitStream.bitIndex(buf.length, currentByte, currentBit))
            check(oldThis1.buf == buf && arr1.length == arr.length)

            arrayUpdatedAtPrefixLemma(arr1, i, b)
            arrayRangesEqTransitive(arr1, arr2, arr, 0, i, i + 1)
            check(arrayRangesEq(arr1, arr, 0, i))

            arrayRangesEqImpliesEq(arr2, arr, 0, i, i + 1)
            check(arr(i) == b)
         }
      } else {
         ghostExpr {
            check(BitStream.bitIndex(oldThis1.buf.length, oldThis1.currentByte, oldThis1.currentBit ) == BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit))
            arrayRangesEqReflexiveLemma(arr)
            arrayRangesEqSlicedLemma(arr, snapshot(arr), 0, arr.length, 0, i)
         }
      }
   }.ensuring { _ =>
      BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit) + (to - i) * 8L == BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit)
      && old(this).buf == this.buf
      && old(arr).length == arr.length
      && arrayRangesEq(old(arr), arr, 0, i)
      (i < to) ==> (arr(i) == old(this).readBytePure()._2)
   }

   @ghost @pure
   def readByteArrayLoopPure(arr: Array[UByte], i: Int, to: Int): (BitStream, Array[UByte]) = {
      require(0 <= i && i <= to && to <= arr.length)
      require(BitStream.validate_offset_bytes(buf.length.toLong, currentByte.toLong, currentBit.toLong, to - i))

      val cpy = snapshot(this)
      val arrCpy = snapshot(arr)
      cpy.readByteArrayLoop(arrCpy, i, to)
      (cpy, arrCpy)
   }

   @extern
   def readByteVec(nBytes: Int): Vector[UByte] = {
      require(nBytes >= 0)
      require(BitStream.validate_offset_bytes(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBytes))

      val arr = readByteArray(nBytes)
      Vector.fromScala(arr.toVector)
   }.ensuring { res =>
      BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit ) + nBytes.toLong * 8L == BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit ) &&
      old(this).buf == this.buf &&
      res.length == nBytes
   }

   /**
    * Read nBits from Bitstream into Byte
    *
    * @param nBits get read from the bitstream
    * @return combined bits into Byte
    *
    * Remarks:
    * nBits starts at LSB and goes upward
    *
    * Example:
    * cur bit on Bitstream = 2
    * nBits = 3
    *
    * Bitstream:
    *    x  x  b1 b2 b3
    *    _  _  _  _  _  _  _  _
    *    0  1  2  3  4  5  6  ...
    *
    * Return Val:
    *       MSB                 LSB
    *       |                    |
    * v =   0  0  0  0  0  _  _  _
    *       7  6  5  4  3  2  1  0
    *                      |  |  |
    *                     b1 b2 b3
    *
    * b1 to b3 are extracted from the bitstream
    * and written into v
    *
    */
   def readPartialByte(nBits: Int): UByte = {
      require(nBits >= 1 && nBits < NO_OF_BITS_IN_BYTE)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))

      var v: Byte = 0
      val cb = currentBit
      val totalBits = cb + nBits

      if totalBits <= 8 then
         v = wrappingExpr { ((buf(currentByte) >>> (8 - totalBits)) & MASK_B(nBits)).toByte }
         moveBitIndex(nBits)
      else
         val totalBitsForNextByte = totalBits - 8
         v = wrappingExpr { (buf(currentByte) << totalBitsForNextByte).toByte }
         currentByte += 1
         v = wrappingExpr { (v | ((buf(currentByte) & 0xFF) >>> (8 - totalBitsForNextByte))).toByte }
         v = wrappingExpr { (v & MASK_B(nBits)).toByte }
         currentBit = totalBitsForNextByte
      UByte.fromRaw(v)
   }.ensuring(_ =>
      buf == old(this).buf
      && BitStream.bitIndex(buf.length, currentByte, currentBit) == BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit) + nBits)

   @pure @ghost
   def readPartialBytePure(nBits: Int): (BitStream, UByte) = {
      require(nBits >= 1 && nBits < NO_OF_BITS_IN_BYTE)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))
      val cpy = snapshot(this)
      val b = cpy.readPartialByte(nBits)
      (cpy, b)
   }

   def checkBitPatternPresent(bit_terminated_pattern: Array[UByte], nBits: Long): Boolean = {
      require(nBits >= 0)
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong, nBits))
      val tmpByte = currentByte
      val tmpBit = currentBit

      val ret = arraySameElements(bit_terminated_pattern, readBits(nBits))
      // TODO: The C code does indeed not reset the stream to the original position
      // in case we return 0 or 1, but doesn't it make sense to do so?
      // if !ret then

      ghostExpr(check(BitStream.invariant(currentBit, currentByte, buf.length)))

      // SAM: Here the issue is that the 2 assignments are not atomic, so the invariant can be temporarily violated
      // For this reason it'd be better to have it as pre- and post condition everywhere rather than relying on the `require`
      // Otherwise, we could store currentByte and currentBit in a tuple
      if(tmpByte == this.buf.length) {
         ghostExpr({
            check(this.currentBit == 0)
            check(tmpBit == 0)
            check(BitStream.invariant(tmpBit, currentByte, buf.length))
         })
         currentBit = tmpBit
         currentByte = tmpByte
      } else {
         currentByte = tmpByte
         currentBit = tmpBit
      }

      ret
   }.ensuring(_ => old(this) == this)


/* is broken, not used and therefore commented
   // NOTE: needs smt-cvc5 and a timeout of at least 50s
   def readBitsUntilTerminator(terminatorPattern: Array[UByte], nBitsTerminator: Long, nMaxReadBits: Long): (Array[UByte], Long) = {
      require(nBitsTerminator >= 0 && nBitsTerminator <= terminatorPattern.length.toLong * NO_OF_BITS_IN_BYTE)
      require(nMaxReadBits >= 0 &&& nMaxReadBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE)
      require(validate_offset_bits(nMaxReadBits + nBitsTerminator))

      @ghost val oldThis = snapshot(this)
      var checkBitPatternPresentResult = checkBitPatternPresent(terminatorPattern, nBitsTerminator)

      // round to next full byte
      val tmpBitStreamLength = ((nMaxReadBits + NO_OF_BITS_IN_BYTE - 1) / NO_OF_BITS_IN_BYTE).toInt
      val tmpStr: BitStream = BitStream(Array.fill(tmpBitStreamLength)(0))

      (while (tmpStr.bitIndex() < nMaxReadBits) && !checkBitPatternPresentResult do
         decreases(nMaxReadBits - tmpStr.bitIndex())
         @ghost val oldTmpStr = snapshot(tmpStr)
         @ghost val oldThis2 = snapshot(this)
         assert(remainingBits == oldThis.remainingBits - tmpStr.bitIndex())
         assert(validate_offset_bits(nMaxReadBits + nBitsTerminator - tmpStr.bitIndex()))
         tmpStr.appendBit(readBit())
         assert(tmpStr.remainingBits == oldTmpStr.remainingBits - 1)
         assert(remainingBits == oldThis2.remainingBits - 1)
         assert(remainingBits == oldThis.remainingBits - tmpStr.bitIndex())

         if tmpStr.bitIndex() < nMaxReadBits then
            checkBitPatternPresentResult = checkBitPatternPresent(terminatorPattern, nBitsTerminator)
         assert(buf == oldThis.buf)
         assert(tmpStr.buf.length == tmpBitStreamLength)
         assert(tmpStr.bitIndex() <= nMaxReadBits)
         assert(tmpStr.remainingBits == tmpBitStreamLength.toLong * NO_OF_BITS_IN_BYTE - tmpStr.bitIndex())
         assert(validate_offset_bits(nMaxReadBits + nBitsTerminator - tmpStr.bitIndex()))
      ).invariant(tmpStr.buf.length == tmpBitStreamLength &&&
         buf == oldThis.buf &&&
         tmpStr.bitIndex() <= nMaxReadBits &&&
         remainingBits == oldThis.remainingBits - tmpStr.bitIndex() &&&
         tmpStr.remainingBits == tmpBitStreamLength.toLong * NO_OF_BITS_IN_BYTE - tmpStr.bitIndex() &&&
         validate_offset_bits(nMaxReadBits + nBitsTerminator - tmpStr.bitIndex()))

      if (tmpStr.bitIndex() == nMaxReadBits) && !checkBitPatternPresentResult then
         checkBitPatternPresentResult = checkBitPatternPresent(terminatorPattern, nBitsTerminator)

      // TODO: Shouldn't we do something about checkBitPatternPresentResult?
      (tmpStr.buf, tmpStr.bitIndex())
   }
   */

   // ************** Aligning functions *********
   def alignToByte(): Unit = {
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong,
         (NO_OF_BITS_IN_BYTE - currentBit) & (NO_OF_BITS_IN_BYTE - 1)
      ))

      if currentBit != 0 then
         currentBit = 0
         currentByte += 1
   }.ensuring(_ => this.buf == old(this).buf && BitStream.bitIndex(this.buf.length, this.currentByte, this.currentBit) <= BitStream.bitIndex(old(this).buf.length, old(this).currentByte, old(this).currentBit) + 7)

   @pure @ghost
   def withAlignedToByte(): BitStream = {
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong,
         (NO_OF_BITS_IN_BYTE - currentBit) & (NO_OF_BITS_IN_BYTE - 1)
      ))
      val cpy = snapshot(this)
      cpy.alignToByte()
      cpy
   }

   def alignToShort(): Unit = {
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong,
         (NO_OF_BITS_IN_SHORT -                                                                 // max alignment (16) -
            (NO_OF_BITS_IN_BYTE * (currentByte & (NO_OF_BYTES_IN_JVM_SHORT - 1)) + currentBit)  // current pos
            ) & (NO_OF_BITS_IN_SHORT - 1))                                                      // edge case (0,0) -> 0
      )

      alignToByte()
      currentByte = ((currentByte + (NO_OF_BYTES_IN_JVM_SHORT - 1)) / NO_OF_BYTES_IN_JVM_SHORT) * NO_OF_BYTES_IN_JVM_SHORT
   }

   @pure @ghost
   def withAlignedToShort(): BitStream = {
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong,
         (NO_OF_BITS_IN_SHORT -                                                                 // max alignment (16) -
            (NO_OF_BITS_IN_BYTE * (currentByte & (NO_OF_BYTES_IN_JVM_SHORT - 1)) + currentBit)  // current pos
            ) & (NO_OF_BITS_IN_SHORT - 1))                                                      // edge case (0,0) -> 0
      )

      val cpy = snapshot(this)
      cpy.alignToShort()
      cpy
   }

   def alignToInt(): Unit = {
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong,
         (NO_OF_BITS_IN_INT -                                                                // max alignment (32) -
            (NO_OF_BITS_IN_BYTE * (currentByte & (NO_OF_BYTES_IN_JVM_INT - 1)) + currentBit) // current pos
            ) & (NO_OF_BITS_IN_INT - 1))                                                     // edge case (0,0) -> 0
      )

      alignToByte()
      currentByte = ((currentByte + (NO_OF_BYTES_IN_JVM_INT - 1)) / NO_OF_BYTES_IN_JVM_INT) * NO_OF_BYTES_IN_JVM_INT
   }

   @pure @ghost
   def withAlignedToInt(): BitStream = {
      require(BitStream.validate_offset_bits(buf.length.toLong, currentByte.toLong, currentBit.toLong,
         (NO_OF_BITS_IN_INT -                                                                // max alignment (32) -
            (NO_OF_BITS_IN_BYTE * (currentByte & (NO_OF_BYTES_IN_JVM_INT - 1)) + currentBit) // current pos
            ) & (NO_OF_BITS_IN_INT - 1))                                                     // edge case (0,0) -> 0
      )

      val cpy = snapshot(this)
      cpy.alignToInt()
      cpy
   }
} // BitStream class
