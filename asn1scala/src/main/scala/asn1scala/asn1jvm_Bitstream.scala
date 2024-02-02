package asn1scala

import stainless.*
import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}
import stainless.collection.*
import stainless.annotation.*
import stainless.proof.*
import stainless.math.{wrapping => wrappingExpr, *}
import StaticChecks.*

object BitStream {
   @pure @inline @ghost
   def invariant(bs: BitStream): Boolean = {
      invariant(bs.currentBit, bs.currentByte, bs.buf.length)
   }

   @pure @ghost
   def invariant(currentBit: Int, currentByte: Int, buffLength: Int): Boolean = {
         currentBit >= 0 && currentBit < NO_OF_BITS_IN_BYTE &&
         currentByte >= 0 && ((currentByte < buffLength) || (currentBit == 0 && currentByte == buffLength))
   }

   @ghost @pure
   def reader(w1: BitStream, w2: BitStream): (BitStream, BitStream) = {
      require(w1.isPrefixOf(w2))
      val r1 = BitStream(snapshot(w2.buf), w1.currentByte, w1.currentBit)
      val r2 = BitStream(snapshot(w2.buf), w2.currentByte, w2.currentBit)
      (r1, r2)
   }

   @ghost @pure @opaque @inlineOnce
   def resetAndThenMovedLemma(b1: BitStream, b2: BitStream, moveInBits: Long): Unit = {
      require(b1.buf.length == b2.buf.length)
      require(moveInBits >= 0)
      require(b1.validate_offset_bits(moveInBits))

      val b2Reset = b2.resetAt(b1)

      {
         ()
      }.ensuring(_ => moveBitIndexPrecond(b2Reset, moveInBits))
   }

   @ghost @pure @opaque @inlineOnce
   def validateOffsetBitsDifferenceLemma(b1: BitStream, b2: BitStream, b1ValidateOffsetBits: Long, b1b2Diff: Long): Unit = {
      require(b1.buf.length == b2.buf.length)
      require(0 <= b1ValidateOffsetBits && 0 <= b1b2Diff && b1b2Diff <= b1ValidateOffsetBits)
      require(b1.validate_offset_bits(b1ValidateOffsetBits))
      require(b1.bitIndex() + b1b2Diff == b2.bitIndex())

      {
         remainingBitsBitIndexLemma(b1)
         assert(b1.remainingBits == b1.buf.length.toLong * NO_OF_BITS_IN_BYTE - b1.bitIndex())
         assert(b1.bitIndex() <= b1.buf.length.toLong * NO_OF_BITS_IN_BYTE - b1ValidateOffsetBits)

         remainingBitsBitIndexLemma(b2)
         assert(b2.remainingBits == b2.buf.length.toLong * NO_OF_BITS_IN_BYTE - (b1.bitIndex() + b1b2Diff))
         assert(b2.remainingBits >= b1ValidateOffsetBits - b1b2Diff)
      }.ensuring(_ => b2.validate_offset_bits(b1ValidateOffsetBits - b1b2Diff))
   }


   @ghost @pure @opaque @inlineOnce
   def remainingBitsBitIndexLemma(b: BitStream): Unit = {
      ()
   }.ensuring(_ => b.remainingBits == b.buf.length.toLong * NO_OF_BITS_IN_BYTE - b.bitIndex())

   @ghost @pure @opaque @inlineOnce
   def validateOffsetBytesContentIrrelevancyLemma(b1: BitStream, buf: Array[Byte], bytes: Int): Unit = {
      require(b1.buf.length == buf.length)
      require(bytes >= 0)
      require(b1.validate_offset_bytes(bytes))
      val b2 = BitStream(snapshot(buf), b1.currentByte, b1.currentBit)

      {
         ()
      }.ensuring(_ => b2.validate_offset_bytes(bytes))
   }

   @ghost @pure @opaque @inlineOnce
   def validateOffsetBitsContentIrrelevancyLemma(b1: BitStream, buf: Array[Byte], bits: Int): Unit = {
      require(b1.buf.length == buf.length)
      require(bits >= 0)
      require(b1.validate_offset_bits(bits))
      val b2 = BitStream(snapshot(buf), b1.currentByte, b1.currentBit)

      {
         ()
      }.ensuring(_ => b2.validate_offset_bits(bits))
   }

   @ghost @pure @opaque @inlineOnce
   def readBytePrefixLemma(bs1: BitStream, bs2: BitStream): Unit = {
      require(bs1.buf.length <= bs2.buf.length)
      require(bs1.bitIndex() + 8 <= bs1.buf.length.toLong * 8L)
      require(bs1.bitIndex() + 8 <= bs2.bitIndex())
      require(arrayBitRangesEq(
         bs1.buf,
         bs2.buf,
         0,
         bs1.bitIndex() + 8
      ))

      val bs2Reset = BitStream(snapshot(bs2.buf), bs1.currentByte, bs1.currentBit)
      val (bs1Res, b1) = bs1.readBytePure()
      val (bs2Res, b2) = bs2Reset.readBytePure()

      {
         val end = (bs1.bitIndex() / 8 + 1).toInt
         arrayRangesEqImpliesEq(bs1.buf, bs2.buf, 0, bs1.currentByte, end)
      }.ensuring { _ =>
         bs1Res.bitIndex() == bs2Res.bitIndex() && b1 == b2
      }
   }

   @ghost @pure @opaque @inlineOnce
   def validTransitiveLemma(w1: BitStream, w2: BitStream, w3: BitStream): Unit = {
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
         val res = b.bitIndex() + diffInBits
         0 <= res && res <= 8 * b.buf.length.toLong
      }
   }
}

private val BitAccessMasks: Array[UByte] = Array(
   -0x80, // -128 / 1000 0000 / x80
   0x40, //   64 / 0100 0000 / x40
   0x20, //   32 / 0010 0000 / x20
   0x10, //   16 / 0001 0000 / x10
   0x08, //    8 / 0000 1000 / x08
   0x04, //    4 / 0000 0100 / x04
   0x02, //    2 / 0000 0010 / x02
   0x01, //    1 / 0000 0001 / x01
)

case class BitStream private [asn1scala](
                       var buf: Array[Byte],
                       var currentByte: Int = 0, // marks the currentByte that gets accessed
                       var currentBit: Int = 0,  // marks the next bit that gets accessed
                    ) { // all BisStream instances satisfy the following:
   import BitStream.*
   require(BitStream.invariant(currentBit, currentByte, buf.length))


   @pure
   def remainingBits: Long = {
      (buf.length.toLong * NO_OF_BITS_IN_BYTE) - (currentByte.toLong * NO_OF_BITS_IN_BYTE + currentBit)
   }

   @pure
   def validate_offset_bit(): Boolean = {
      remainingBits >= 1
   }.ensuring(_ => BitStream.invariant(this))

   @pure
   def validate_offset_bits(bits: Long = 0): Boolean = {
      require(bits >= 0)
      remainingBits >= bits
   }.ensuring(_ => BitStream.invariant(this))

   @pure
   def validate_offset_byte(): Boolean = {
      remainingBits >= NO_OF_BITS_IN_BYTE
   }.ensuring(_ => BitStream.invariant(this))

   @pure
   def validate_offset_bytes(bytes: Int): Boolean = {
      require(bytes >= 0)
      bytes <= remainingBits / NO_OF_BITS_IN_BYTE
   }.ensuring(_ => BitStream.invariant(this))

   @pure
   def bitIndex(): Long = {
      currentByte.toLong * NO_OF_BITS_IN_BYTE + currentBit
   }.ensuring(res =>
         res == buf.length.toLong * NO_OF_BITS_IN_BYTE - remainingBits
   )

   @pure
   def isPrefixOf(b2: BitStream): Boolean = {
      buf.length <= b2.buf.length &&
      bitIndex() <= b2.bitIndex() &&
      (buf.length != 0) ==> arrayBitRangesEq(buf, b2.buf, 0, bitIndex())
   }

   def resetBitIndex(): Unit = {
      currentBit = 0
      currentByte = 0
   }

   private def increaseBitIndex(): Unit = {
      require(remainingBits > 0)
      if currentBit < NO_OF_BITS_IN_BYTE - 1 then
         currentBit += 1
      else
         currentBit = 0
         currentByte += 1

   }.ensuring {_ =>
      val oldBitStr = old(this)
      oldBitStr.bitIndex() + 1 == this.bitIndex() &&&
         oldBitStr.remainingBits - remainingBits == 1 &&&
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
   }.ensuring(_ => old(this).bitIndex() + diffInBits == bitIndex())

   @ghost @pure
   def withMovedBitIndex(diffInBits: Int): BitStream = {
      require(moveBitIndexPrecond(this, diffInBits))
      val cpy = snapshot(this)
      cpy.moveBitIndex(diffInBits)
      cpy
   }

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
   def getLength: Int = {
      var ret: Int = currentByte
      if currentBit > 0 then
         ret += 1
      ret
   }

   @ghost @pure @inline
   def getBuf: Array[Byte] = buf

   @ghost @pure @inline
   def resetAt(b: BitStream): BitStream =
      BitStream(snapshot(buf), b.currentByte, b.currentBit)

//   @ghost
//   @pure
//   private def readBitPure(): (BitStream, Option[Boolean]) = {
//      require(validate_offset_bit())
//      val cpy = snapshot(this)
//      (cpy, cpy.readBit())
//   }

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
   def appendBit(b: Boolean): Unit = {
      require(validate_offset_bit())
      if b then
         buf(currentByte) = (buf(currentByte) | BitAccessMasks(currentBit)).toByte
      else
         buf(currentByte) = (buf(currentByte) & (~BitAccessMasks(currentBit))).toByte

      increaseBitIndex()
   }.ensuring(_ => buf.length == old(this).buf.length && remainingBits == old(this).remainingBits - 1) // NOTE: needs cvc5 for solving

   /**
    * Append a set bit
    */
   def appendBitOne(): Unit = {
      require(validate_offset_bit())

      appendBit(true)
   }.ensuring(_ => buf.length == old(this).buf.length && remainingBits == old(this).remainingBits - 1)

   /**
    * Append n set bits to bitstream
    *
    * @param nBits number of bits
    *
    */
   def appendNOneBits(nBits: Long): Unit = {
      require(nBits >= 0)
      require(validate_offset_bits(nBits))

      @ghost val oldThis = snapshot(this)
      var i = 0L
      (while i < nBits do
         decreases(nBits - i)
         appendBitOne()
         i += 1
      ).invariant(
         0 <= i && i <= nBits &&
         nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong &&
         buf.length == oldThis.buf.length &&
         remainingBits == oldThis.remainingBits - i &&
         validate_offset_bits(nBits - i))

   }.ensuring(_ => buf.length == old(this).buf.length && remainingBits == old(this).remainingBits - nBits)

   /**
    * Append cleared bit to bitstream
    */
   def appendBitZero(): Unit = {
      require(validate_offset_bit())

      appendBit(false)
   }.ensuring(_ => buf.length == old(this).buf.length && remainingBits == old(this).remainingBits - 1)

   /**
    * Append n cleared bits to bitstream
    *
    * @param nBits number of bits
    *
    */
   def appendNZeroBits(nBits: Long): Unit = {
      require(nBits >= 0)
      require(validate_offset_bits(nBits))

      @ghost val oldThis = snapshot(this)
      var i = 0L
      (while i < nBits do
         decreases(nBits - i)
         appendBitZero()
         i += 1
      ).invariant(
         0 <= i && i <= nBits &&
         nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong &&
         buf.length == oldThis.buf.length &&
         remainingBits == oldThis.remainingBits - i &&
         validate_offset_bits(nBits - i))

   }.ensuring(_ => buf.length == old(this).buf.length && remainingBits == old(this).remainingBits - nBits)

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
      require(validate_offset_bit())

      val bitPosInByte = 1 << ((NO_OF_BITS_IN_BYTE - 1) - bitNr)
      appendBit((b.unsignedToInt & bitPosInByte) != 0)

   }.ensuring(_ => buf.length == old(this).buf.length && remainingBits == old(this).remainingBits - 1)

   /**
    * Append nBits from the 64bit Integer value v to the bitstream
    *
    * @param v source of the bits
    * @param nBits number of bits to add
    *
    * Remarks:
    * bit 0 is the LSB of v
    */
   def appendBitsLSBFirst(v: Long, nBits: Int): Unit = {
      require(nBits >= 0 && nBits <= NO_OF_BITS_IN_LONG)
      require(validate_offset_bits(nBits))

//      @ghost val oldThis = snapshot(this)
      var i = 0
      (while i < nBits do
         decreases(nBits - i)

         val ii = v & (1L << i)
         val b = ii != 0

         appendBit(b)

         i += 1
      ).invariant(i >= 0 && i <= nBits &&& validate_offset_bits(nBits - i))
   }

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
   def appendNLeastSignificantBits(v: Long, nBits: Int): Unit = {
      require(nBits >= 0 && nBits <= NO_OF_BITS_IN_LONG)
      require(validate_offset_bits(nBits))

      var i = nBits - 1
      (while i >= 0 do
         decreases(i)

         val ii = v & (1L << i)
         val b = ii != 0

         appendBit(b)

         i -= 1
         ).invariant(i >= -1 && i <= nBits &&& validate_offset_bits(i+1))
   }

   /**
    * Append nBits from srcBuffer to bitstream
    *
    * @param srcBuffer source of the bits to add
    * @param nBits number of bits to add
    *
    * Remarks:
    * bit 0 is the MSB of the first byte of srcBuffer
    *
    */
   def appendBitsMSBFirst(srcBuffer: Array[UByte], nBits: Long): Unit = {
      require(nBits >= 0 && (nBits / 8) < srcBuffer.length)
      require(validate_offset_bits(nBits))

      @ghost val oldThis = snapshot(this)
      var i = 0L
      (while i < nBits do
         decreases(nBits - i)

         appendBitFromByte(srcBuffer((i / NO_OF_BITS_IN_BYTE).toInt), (i % NO_OF_BITS_IN_BYTE).toInt)

         i += 1L
      ).invariant(i >= 0 &&& i <= nBits &&& i / NO_OF_BITS_IN_BYTE <= Int.MaxValue &&&
         buf.length == oldThis.buf.length &&&
         remainingBits == oldThis.remainingBits - i &&&
         validate_offset_bits(nBits - i))

   }.ensuring(_ => buf.length == old(this).buf.length && remainingBits == old(this).remainingBits - nBits)

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
      require(validate_offset_bits(nBits))

      @ghost val oldThis = snapshot(this)

      val cb = currentBit
      val totalBits = cb + nBits
      val ncb = 8 - cb

      val mask1 = (~MASK_B(ncb)).toByte
      val vv = (v & MASK_B(nBits)).toByte
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
         w2.bitIndex() == w1.bitIndex() + nBits && w1.isPrefixOf(w2) && {
         val (r1, r2) = reader(w1, w2)
         val (r2Got, vGot) = r1.readPartialBytePure(nBits)
         vGot == wrappingExpr { (v & MASK_B(nBits)).toByte } && r2Got == r2
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
      require(validate_offset_bytes(1))

      @ghost val oldThis = snapshot(this)
      val cb = currentBit.toByte
      val ncb = (8 - cb).toByte
      var mask = (~MASK_B(ncb)).toByte

      buf(currentByte) = wrappingExpr { (buf(currentByte) & mask).toByte }
      buf(currentByte) = wrappingExpr { (buf(currentByte) | ((v & 0xFF) >>> cb)).toByte }
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
         buf(currentByte) = wrappingExpr { (buf(currentByte) | (v << ncb)).toByte }

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
      w1.buf.length == w2.buf.length && w2.bitIndex() == w1.bitIndex() + 8 && w1.isPrefixOf(w2) && {
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
      require(validate_offset_bytes(noOfBytes))

      @ghost val oldThis = snapshot(this)
      var i: Int = 0
      (while i < noOfBytes do
         decreases(noOfBytes - i)

         appendByte(arr(i))

         i += 1
      ).invariant(
         0 <= i && i <= noOfBytes &&
         buf.length == oldThis.buf.length &&
         remainingBits == oldThis.remainingBits - i.toLong * NO_OF_BITS_IN_BYTE &&
         validate_offset_bytes(noOfBytes - i))

   }.ensuring(_ => buf.length == old(this).buf.length && remainingBits == old(this).remainingBits - noOfBytes.toLong * NO_OF_BITS_IN_BYTE)

   // ****************** Peak Functions **********************

   /**
    * Preview the next bit on the bitstream
    *
    * @return peeked bit
    *
    */
   @pure
   def peekBit(): Boolean = {
      require(validate_offset_bit())
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
      require(BitStream.invariant(this))
      require(validate_offset_bit())
      val ret = (buf(currentByte) & BitAccessMasks(currentBit)) != 0

      increaseBitIndex()

      ret
   }.ensuring(_ => buf == old(this).buf && remainingBits == old(this).remainingBits - 1)

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
      require(nBits >= 0 && validate_offset_bits(nBits))
      assert(nBits <= Int.MaxValue.toLong * NO_OF_BITS_IN_BYTE.toLong)
      val arrLen = ((nBits + NO_OF_BITS_IN_BYTE - 1) / NO_OF_BITS_IN_BYTE).toInt
      val arr: Array[UByte] = Array.fill(arrLen)(0)

      @ghost val oldThis = snapshot(this)
      var i = 0L
      (while i < nBits  do
         decreases(nBits - i)

         arr((i / NO_OF_BITS_IN_BYTE).toInt) |||= (if readBit() then BitAccessMasks((i % NO_OF_BITS_IN_BYTE).toInt) else 0)

         i += 1
      ).invariant(i >= 0 &&& i <= nBits &&& validate_offset_bits(nBits - i) &&&
         (i / NO_OF_BITS_IN_BYTE) <= Int.MaxValue &&&
         buf == oldThis.buf &&&
         remainingBits == oldThis.remainingBits - i &&&
         arr.length == arrLen)

      arr
   }.ensuring(_ => buf == old(this).buf && remainingBits == old(this).remainingBits - nBits)

   /**
    * Counter Operation to appendNLeastSignificantBits
    * @param nBits number of bits to read [0-64]
    * @return value that holds nBits from bitstream
    *
    * Remarks:
    * The last bit from the bitstream will get written into the LSB
    */
   def readNLeastSignificantBits(nBits: Int): Long = {
      require(nBits >= 0 && nBits <= 64)
      require(validate_offset_bits(nBits))

      var l: Long = 0

      @ghost val oldThis = snapshot(this)
      var i = 0
      (while i < nBits do
         decreases(nBits - i)

         l |= (if readBit() then 1L << (nBits - 1 - i) else 0)

         i += 1
      ).invariant(
         i >= 0 && i <= nBits &&&
            validate_offset_bits(nBits - i) &&&
            buf == oldThis.buf &&&
            remainingBits == oldThis.remainingBits - i)

      l
   }.ensuring(_ => buf == old(this).buf && remainingBits == old(this).remainingBits - nBits.toLong)

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
      require(validate_offset_bits(8))

      val cb = currentBit.toByte
      val ncb = (8 - cb).toByte
      var v = wrappingExpr { (buf(currentByte) << cb).toByte }
      currentByte += 1

      if cb > 0 then
         v = wrappingExpr { (v | (buf(currentByte) & 0xFF) >>> ncb).toByte }
      v
   }.ensuring(_ => buf == old(this).buf && bitIndex() == old(this).bitIndex() + 8)

   @ghost @pure
   def readBytePure(): (BitStream, UByte) = {
      require(validate_offset_bits(8))
      val cpy = snapshot(this)
      val res = cpy.readByte()
      (cpy, res)
   }

   def readByteArray(nBytes: Int): Array[UByte] = {
      require(nBytes <= Integer.MAX_VALUE / NO_OF_BITS_IN_BYTE)
      require(nBytes >= 0 && nBytes <= remainingBits / NO_OF_BITS_IN_BYTE)
      require(validate_offset_bytes(nBytes))

      // @ghost val oldThis = snapshot(this)
      // val nBits = (nBytes * NO_OF_BITS_IN_BYTE).toLong
      val res = readBits(nBytes * NO_OF_BITS_IN_BYTE)
      // assert(remainingBits == oldThis.remainingBits - nBits)
      // assert(nBits == nBytes.toLong * NO_OF_BITS_IN_BYTE)
      res
   }.ensuring(_ => buf == old(this).buf && remainingBits == old(this).remainingBits - nBytes.toLong * NO_OF_BITS_IN_BYTE)


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
      require(validate_offset_bits(nBits))

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
      v
   }.ensuring(_ => buf == old(this).buf && remainingBits == old(this).remainingBits - nBits)

   @pure @ghost
   def readPartialBytePure(nBits: Int): (BitStream, UByte) = {
      require(nBits >= 1 && nBits < NO_OF_BITS_IN_BYTE)
      require(validate_offset_bits(nBits))
      val cpy = snapshot(this)
      val b = cpy.readPartialByte(nBits)
      (cpy, b)
   }

   def checkBitPatternPresent(bit_terminated_pattern: Array[UByte], nBits: Long): Boolean = {
      require(nBits >= 0)
      require(validate_offset_bits(nBits))
      val tmpByte = currentByte
      val tmpBit = currentBit

      val ret = arraySameElements(bit_terminated_pattern, readBits(nBits))
      // TODO: The C code does indeed not reset the stream to the original position
      // in case we return 0 or 1, but doesn't it make sense to do so?
      // if !ret then
      currentByte = tmpByte
      currentBit = tmpBit

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
      require(validate_offset_bits(
         (NO_OF_BITS_IN_BYTE - currentBit) & (NO_OF_BITS_IN_BYTE - 1)
      ))

      if currentBit != 0 then
         currentBit = 0
         currentByte += 1
   }

   def alignToShort(): Unit = {
      require(validate_offset_bits(
         (NO_OF_BITS_IN_SHORT -                                                                 // max alignment (16) -
            (NO_OF_BITS_IN_BYTE * (currentByte & (NO_OF_BYTES_IN_JVM_SHORT - 1)) + currentBit)  // current pos
            ) & (NO_OF_BITS_IN_SHORT - 1))                                                      // edge case (0,0) -> 0
      )

      alignToByte()
      currentByte = ((currentByte + (NO_OF_BYTES_IN_JVM_SHORT - 1)) / NO_OF_BYTES_IN_JVM_SHORT) * NO_OF_BYTES_IN_JVM_SHORT
   }

   def alignToInt(): Unit = {
      require(validate_offset_bits(
         (NO_OF_BITS_IN_INT -                                                                // max alignment (32) -
            (NO_OF_BITS_IN_BYTE * (currentByte & (NO_OF_BYTES_IN_JVM_INT - 1)) + currentBit) // current pos
            ) & (NO_OF_BITS_IN_INT - 1))                                                     // edge case (0,0) -> 0
      )

      alignToByte()
      currentByte = ((currentByte + (NO_OF_BYTES_IN_JVM_INT - 1)) / NO_OF_BYTES_IN_JVM_INT) * NO_OF_BYTES_IN_JVM_INT
   }
} // BitStream class
