package asn1scala

import stainless.*
import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}
import stainless.collection.*
import stainless.annotation.*
import stainless.proof.*
import stainless.math.*
import StaticChecks.*

// TODO should be part of BitStream
def isPrefix(b1: BitStream, b2: BitStream): Boolean = {
   b1.buf.length <= b2.buf.length &&
     b1.bitIndex() <= b2.bitIndex() &&
     (b1.buf.length != 0) ==> arrayBitPrefix(b1.buf, b2.buf, 0, b1.bitIndex())
}

def isValidPair(w1: BitStream, w2: BitStream): Boolean = isPrefix(w1, w2)

@ghost
def reader(w1: BitStream, w2: BitStream): (BitStream, BitStream) = {
   require(isValidPair(w1, w2))
   val r1 = BitStream(snapshot(w2.buf), w1.currentByte, w1.currentBit)
   val r2 = BitStream(snapshot(w2.buf), w2.currentByte, w2.currentBit)
   (r1, r2)
}

//@ghost @pure
//def readBytePure(pBitStrm: BitStream): (BitStream, Option[Byte]) = {
//   require(pBitStrm.validate_offset_bytes(1))
//   val cpy = snapshot(pBitStrm)
//   (cpy, cpy.readByte())
//}

// END TODO should be part of BitStream


object BitStream {
   @pure @inline
   def invariant(pBitStrm: BitStream): Boolean = {
      BitStream.invariant(pBitStrm.currentByte, pBitStrm.currentBit, pBitStrm.buf.length)
   }

   @pure @inline
   def invariant(currentByte: Int, currentBit: Int, buf_length: Int): Boolean = {
      0 <= currentBit && currentBit <= 7 &&&
         0 <= currentByte && (currentByte < buf_length || (currentBit == 0 && currentByte <= buf_length))
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

case class BitStream(
                       var buf: Array[Byte],
                       var currentByte: Int = 0, // marks the currentByte that gets accessed
                       var currentBit: Int = 0,  // marks the next bit that gets accessed
                    ) { // all BisStream instances satisfy the following:
   require(BitStream.invariant(currentByte, currentBit, buf.length))

   private var remainingBits: Long = buf.length.toLong * 8

   @ghost
   def validate_offset_bit(): Boolean = {
      remainingBits >= 1
   }

   @ghost
   def validate_offset_bits(bits: Int = 0): Boolean = {
      require(bits >= 0)
      remainingBits >= bits
   }

   @ghost
   def validate_offset_byte(): Boolean = {
      remainingBits >= 8
   }

   @ghost
   def validate_offset_bytes(bytes: Int = 0): Boolean = {
      require(bytes >= 0)
      remainingBits >= bytes.toLong * 8
   }

   def bitIndex(): Long = {
      currentByte.toLong * 8 + currentBit.toLong
   }.ensuring(res => 0 <= res && res <= 8 * buf.length.toLong &&& res == buf.length.toLong * 8 - remainingBits)

   def increaseBitIndex(): Unit = {
      require(currentByte < buf.length)
      if currentBit < 7 then
         currentBit += 1
      else
         currentBit = 0
         currentByte += 1

      remainingBits -= 1
   }.ensuring {_ =>
      val oldBitStrm = old(this)
      oldBitStrm.bitIndex() + 1 == this.bitIndex() &&&
         BitStream.invariant(this)
   }

   // TODO not used
   @inlineOnce @opaque @ghost
   private def ensureInvariant(): Unit = {
   }.ensuring(_ =>
      BitStream.invariant(currentByte, currentBit, buf.length)
   )

   /**
    * Set new internal buffer
    *
    * @param buf Byte array that should be attached to this BitStream
    *
    */
   @extern
   def attachBuffer(buf: Array[UByte]): Unit = {
      this.buf = buf // Illegal aliasing, therefore we need to workaround with this @extern...
      currentByte = 0
      currentBit = 0
      remainingBits = buf.length.toLong * 8
   }.ensuring(_ => this.buf == buf && currentByte == 0 && currentBit == 0)

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
   def getLength(): Int = {
      var ret: Int = currentByte
      if currentBit > 0 then
         ret += 1
      ret
   }

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
   }.ensuring(_ => BitStream.invariant(this))

   /**
    * Append a set bit
    */
   def appendBitOne(): Unit = {
      require(validate_offset_bit())

      appendBit(true)
   }

   /**
    * Append n set bits to bitstream
    *
    * @param nBits number of bits
    *
    */
   def appendNBitOne(nBits: Int): Unit = {
      require(nBits >= 0)
      require(validate_offset_bits(nBits))

      var i = 0
      (while i < nBits do
         decreases(nBits - i)

         appendBitOne()

         i += 1
      ).invariant(i >= 0 &&& i <= nBits)
   }.ensuring(_ => BitStream.invariant(this))

   /**
    * Append cleared bit to bitstream
    */
   def appendBitZero(): Unit = {
      require(validate_offset_bit())

      appendBit(false)
   }

   /**
    * Append n cleared bit to bitstream
    *
    * @param nBits number of bits
    *
    */
   def appendNBitZero(nBits: Int): Unit = {
      require(nBits >= 0)
      require(validate_offset_bits(nBits))

      var i = 0
      (while i < nBits do
         decreases(nBits - i)

         appendBitZero()

         i += 1
      ).invariant(i >= 0 &&& i <= nBits)

   }.ensuring(_ => BitStream.invariant(this))

   /**
    * Append bit with bitNr from b to bitstream
    *
    * @param b byte that gets the bit extracted from
    * @param bitNr 0 to 7 - number of the bit
    *
    * Remarks:
    * bit 0 is the MSB, bit 7 is the LSB, ASN.1? / ESA? declares bit 1 as MSB,
    * bit 8 as LSB - but we start from 0 in CS
    *
    */
   private def appendBitFromByte(b: Byte, bitNr: Int): Unit = {
      require(bitNr >= 0 && bitNr < NO_OF_BITS_IN_BYTE)
      require(validate_offset_bit())

      val bitPosInByte = 1 << ((NO_OF_BITS_IN_BYTE - 1) - bitNr)
      appendBit((b.unsignedToInt & bitPosInByte) != 0)

   }.ensuring(_ => BitStream.invariant(this))

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
   def appendBits(srcBuffer: Array[UByte], nBits: Int): Unit = {
      require(nBits >= 0 && nBits / 8 < srcBuffer.length)
      require(validate_offset_bits(nBits))

      var i = 0
      (while(i < nBits) do
         decreases(nBits - i)

         appendBitFromByte(srcBuffer(i / NO_OF_BITS_IN_BYTE), i % NO_OF_BITS_IN_BYTE)

         i += 1
      ).invariant(i >= 0 &&& i <= nBits &&& validate_offset_bits(nBits - i))
   }.ensuring(_ => BitStream.invariant(this))

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

   def appendPartialByte(v: UByte, nBits: Int): Unit = {
      require(nBits >= 0 &&& nBits <= NO_OF_BITS_IN_BYTE)
      require(validate_offset_bits(nBits))

      var i = 0
      (while i < nBits do
         decreases(nBits - i)

         appendBitFromByte(v, NO_OF_BITS_IN_BYTE - nBits + i)

         i += 1
         ).invariant(i >= 0 && i <= nBits)
   }.ensuring(_ => BitStream.invariant(this))


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
   def appendByte(v: UByte): Unit = {
      require(validate_offset_bytes(1))

      appendPartialByte(v, NO_OF_BITS_IN_BYTE)

   }.ensuring(_ => BitStream.invariant(this))

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

      var i: Int = 0
      (while i < noOfBytes do
         decreases(noOfBytes - i)

         appendByte(arr(i))

         i += 1
      ).invariant(0 <= i &&& i <= noOfBytes &&& validate_offset_bytes(noOfBytes - i))

   }.ensuring(_ => BitStream.invariant(this))

   // ****************** Peak Functions **********************

   /**
    * Preview the next bit on the bitstream
    *
    * @return peeked bit
    *
    */
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
      require(validate_offset_bit())
      val ret = (buf(currentByte) & BitAccessMasks(currentBit)) != 0

      increaseBitIndex()

      ret
   }.ensuring(_ => BitStream.invariant(this))

   /**
    * Read multiple bits from the bitstream
    *
    * @param nBits number of bits to read
    * @return array of read bits
    *
    * Remarks:
    * First bit is written into the MSB of Byte 0
    *
    */
   def readBits(nBits: Int): Array[UByte] = {
      require(nBits > 0 && (nBits <= Integer.MAX_VALUE - NO_OF_BITS_IN_BYTE)) // TODO remaining bits in stream as upper bound, not MAX_VAL
      require(validate_offset_bits(nBits))

      val arr: Array[UByte] = Array.fill((nBits + NO_OF_BITS_IN_BYTE - 1) / NO_OF_BITS_IN_BYTE)(0)

      var i = 0
      (while i < nBits  do
         decreases(nBits - i)

            arr(i / NO_OF_BITS_IN_BYTE) |||= (if readBit() then BitAccessMasks(i % NO_OF_BITS_IN_BYTE) else 0)

         i += 1
      ).invariant(i >= 0 &&& i <= nBits &&& validate_offset_bits(nBits - i))

      arr
   }.ensuring(_ => BitStream.invariant(this))

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

      var ret = 0
      var i = 0
      (while i < NO_OF_BITS_IN_BYTE do
         decreases(NO_OF_BITS_IN_BYTE - i)

         ret |= (if readBit() then BitAccessMasks(i) else 0)
         i += 1
      ).invariant(i >= 0 &&& i <= NO_OF_BITS_IN_BYTE &&& validate_offset_bits(NO_OF_BITS_IN_BYTE - i))

      ret.toUnsignedByte
   }.ensuring(_ => BitStream.invariant(this))

   def readByteArray(nBytes: Int): Array[UByte] = {
      require(0 < nBytes && nBytes <= buf.length)
      require(validate_offset_bytes(nBytes))

      readBits(nBytes * NO_OF_BITS_IN_BYTE)
   }.ensuring(_ => BitStream.invariant(this))


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
      require(0 < nBits && nBits < 8)
      require(validate_offset_bits(nBits))

      var v = 0.toByte
      var i = 0
      (while i < nBits do
         decreases(nBits - i)

         v |||= (if readBit() then BitAccessMasks(NO_OF_BITS_IN_BYTE - nBits + i) else 0)

         i += 1
      ).invariant(i >= 0 && i <= nBits)

      v
   }.ensuring(_ => BitStream.invariant(this))

} // BitStream class
