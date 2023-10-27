package asn1scala

import stainless.*
import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}
import stainless.collection.*
import stainless.annotation.*
import stainless.proof.*
import stainless.math.*
import StaticChecks.*

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

   @ghost
   def validate_offset_bit(pBitStrm: BitStream): Boolean = {
      //        var new_currentBit: Int = pBitStrm.currentBit + 1
      //        var new_currentByte: Int = pBitStrm.currentByte
      //
      //        if new_currentBit > 7 then
      //            new_currentBit = new_currentBit % 8
      //            new_currentByte += 1
      //
      //        0 <= new_currentBit
      //          && new_currentBit <= 7
      //          && 0 <= new_currentByte
      //          && (new_currentByte < pBitStrm.buf.length || (new_currentBit == 0 && pBitStrm.currentByte <= pBitStrm.buf.length))
      pBitStrm.currentByte < pBitStrm.buf.length
   }

   @ghost
   def validate_offset_bits(pBitStrm: BitStream, bits: Int = 0): Boolean = {
      require(0 <= bits)
      val nBits = bits % 8
      val nBytes = bits / 8

      var new_currentByte: Long = pBitStrm.currentByte.toLong + nBytes
      var new_currentBit: Long = pBitStrm.currentBit.toLong + nBits

      if new_currentBit > 7 then
         new_currentBit = new_currentBit % 8
         new_currentByte += 1

      0 <= new_currentBit
         && new_currentBit <= 7
         && 0 <= new_currentByte
         && (new_currentByte < pBitStrm.buf.length || (new_currentBit == 0 && new_currentByte <= pBitStrm.buf.length))
   }

   @ghost
   def validate_offset_bytes(pBitStrm: BitStream, bytes: Int = 0): Boolean = {
      val new_currentByte: Long = pBitStrm.currentByte.toLong + bytes
      new_currentByte < pBitStrm.buf.length || (pBitStrm.currentBit == 0 && new_currentByte <= pBitStrm.buf.length)
   }
}

case class BitStream(
                       var buf: Array[Byte],
                       var currentByte: Int, // marks the currentByte that gets accessed
                       var currentBit: Int,  // marks the next bit that gets accessed
                    ) { // all BisStream instances satisfy the following:
   require(BitStream.invariant(currentByte, currentBit, buf.length))

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

   def bitIndex(): Long = {
      currentByte.toLong * 8 + currentBit.toLong
   }.ensuring(res => 0 <= res && res <= 8 * buf.length.toLong)

   def increaseBitIndex(): Unit = {
      require(currentByte < buf.length)
      if currentBit < 7 then
         currentBit += 1
      else
         currentBit = 0
         currentByte += 1
   }.ensuring {_ =>
      val oldBitStrm = old(this)
      oldBitStrm.bitIndex() + 1 == this.bitIndex() &&&
         BitStream.invariant(this)
   }

   @inlineOnce @opaque @ghost
   def ensureInvariant(): Unit = {
   }.ensuring(_ =>
      BitStream.invariant(currentByte, currentBit, buf.length)
   )

   /**
    * Set new internal buffer
    * @param buf Byte array that should be attached to this BitStream
    */

   @extern
   def attachBuffer(buf: Array[UByte]): Unit = {
      this.buf = buf // Illegal aliasing, therefore we need to workaround with this @extern...
      currentByte = 0
      currentBit = 0
   }.ensuring(_ => this.buf == buf && currentByte == 0 && currentBit == 0)

   /**
    *
    * Return count of bytes that got already fully or partially written
    * Example:
    * Currentbyte = 4, currentBit = 2 --> 5
    * Currentbyte = 14, currentBit = 0 --> 14
    *
    * @return the number of used bytes so far
    *
    */
   def getLength(): Int = {
      var ret: Int = currentByte
      if currentBit > 0 then
         ret += 1
      ret
   }

   @ghost
   @pure
   private def readBitPure(): (BitStream, Option[Boolean]) = {
      require(BitStream.validate_offset_bit(this))
      val cpy = snapshot(this)
      (cpy, cpy.readBit())
   }

   @opaque
   @inlineOnce
   def appendBitOne(): Unit = {
      require(BitStream.validate_offset_bit(this))
      @ghost val oldpBitStrm = snapshot(this)

      val newB = (buf(currentByte) | BitAccessMasks(currentBit)).toByte
      buf(currentByte) = newB

      ghostExpr {
         arrayUpdatedAtPrefixLemma(oldpBitStrm.buf, currentByte, newB)
      }

      increaseBitIndex()

   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      w2.bitIndex() == w1.bitIndex() + 1
         &&& isValidPair(w1, w2)
         &&& {
         val (r1, r2) = reader(w1, w2)
         val (r2Got, bitGot) = readBitPure()
         bitGot.get == true && r2Got == r2
      }
         &&& BitStream.invariant(this)
   }

   /**
    * Append zero bit.
    *
    * Example
    * cur bit = 3
    *  x x x |
    * |_|_|_|_|_|_|_|_|
    *  0 1 2 3 4 5 6 7
    *
    *      x x x y ? ? ? ?
    * and  1 1 1 0 1 1 1 1
    *      ----------------
    * -->  x x x 0 ? ? ? ?
    * */

   // TODO replace with addBit(false)?
   def appendBitZero(): Unit = {
      require(BitStream.validate_offset_bits(this, 1))
      val negMask = ~BitAccessMasks(currentBit)
      buf(currentByte) = (buf(currentByte) & negMask).toByte

      increaseBitIndex()
   }.ensuring(_ => BitStream.invariant(this))


   // TODO what are you? it does not append even though it has append in the name
   // TODO replace with loop that calls appendBitZero
   def appendNBitZero(nBitsVal: Int): Unit = {
      require(0 <= nBitsVal)
      require(BitStream.validate_offset_bits(this, nBitsVal))

      val nBits = nBitsVal % 8
      val nBytes = nBitsVal / 8

      var new_currentBit: Int = currentBit + nBits
      var new_currentByte: Int = currentByte + nBytes

      if new_currentBit > 7 then
         new_currentBit = new_currentBit % 8
         new_currentByte += 1

      currentBit = new_currentBit
      currentByte = new_currentByte

   }.ensuring(_ => BitStream.invariant(this))


   def appendNBitOne(nBitsVal: Int): Unit = {
      require(0 <= nBitsVal)
      require(BitStream.validate_offset_bits(this, nBitsVal))
      var nBits = nBitsVal

      (while nBits > 0 do
         decreases(nBits)
         appendBitOne()
         nBits -= 1
         ).invariant(nBits >= 0 &&& BitStream.validate_offset_bits(this, nBits))
   }

   def appendBits(srcBuffer: Array[UByte], nBits: Int): Unit = {
      require(0 <= nBits && nBits / 8 < srcBuffer.length)
      require(BitStream.validate_offset_bits(this, nBits))
      var lastByte: UByte = 0

      val bytesToEncode: Int = nBits / 8
      val remainingBits: UByte = (nBits % 8).toByte

      BitStream_EncodeOctetString_no_length(this, srcBuffer, bytesToEncode)

      if remainingBits > 0 then
         lastByte = ((srcBuffer(bytesToEncode) & 0xFF) >>> (8 - remainingBits)).toByte
         BitStream_AppendPartialByte(this, lastByte, remainingBits, false)
   }

   def appendBit(v: Boolean): Unit = {
      require(BitStream.validate_offset_bits(this, 1))
      if v then
         buf(currentByte) = (buf(currentByte) | BitAccessMasks(currentBit)).toByte
      else
         val negMask = ~BitAccessMasks(currentBit)
         buf(currentByte) = (buf(currentByte) & negMask).toByte

      increaseBitIndex()
   }.ensuring(_ => BitStream.invariant(this))

   // TODO check if needs Marios implementation

   def readBit(): Option[Boolean] = {
      require(BitStream.validate_offset_bit(this))
      val ret = (buf(currentByte) & BitAccessMasks(currentBit)) != 0

      increaseBitIndex()

      if currentByte.toLong * 8 + currentBit <= buf.length.toLong * 8 then
         Some(ret)
      else
         None()
   }.ensuring(_ => BitStream.invariant(this))

   def peekBit(): Boolean = {
      require(currentByte < buf.length)
      ((buf(currentByte) & 0xFF) & (BitAccessMasks(currentBit) & 0xFF)) > 0
   }

   /**
    * Append byte.
    *
    * Example
    * cur bit = 3
    * |
    * x x x b b b b b b b b
    * |_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
    * 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7
    *
    * first byte
    *       xxx?????
    * and   11100000 (mask)
    * --------------
    *       xxx00000
    * or    000bbbbb
    * --------------
    *       xxxbbbbb
    *
    * */

   @opaque
   @inlineOnce
   def appendByte(value: Byte, negate: Boolean): Unit = {
      require(BitStream.validate_offset_bytes(this, 1))
      @ghost val oldpBitStrm = snapshot(this)
      val cb = currentBit.toByte
      val ncb = (8 - cb).toByte
      var mask = (~masksb(ncb)).toByte

      var v = value
      if negate then
         v = (~v).toByte

      buf(currentByte) = (buf(currentByte) & mask).toByte // set bits right of currentbit to zero (where our value will be inserted)
      buf(currentByte) = (buf(currentByte) | (v >>>> cb)).toByte // set value into bits right of currentbit, but keep bits to the left
      currentByte += 1

      ghostExpr {
         check(
            (oldpBitStrm.currentByte < oldpBitStrm.buf.length) ==>
                bytePrefix(
                   oldpBitStrm.buf(oldpBitStrm.currentByte),
                   buf(oldpBitStrm.currentByte),
                   0, oldpBitStrm.currentBit))
      }
      @ghost val old2pBitStrm = snapshot(this)

      if cb > 0 then
         mask = (~mask).toByte
         buf(currentByte) = (buf(currentByte) & mask).toByte // set bits to the left of currentbit in next byte to zero (where the rest of our value will be inserted)
         buf(currentByte) = (buf(currentByte) | (v <<<< ncb)).toByte // set value into the bits left of currentbit, but keep the bits to the right

      ghostExpr {
         arrayUpdatedAtPrefixLemma(oldpBitStrm.buf, currentByte - 1, buf(currentByte - 1))
         assert(arrayPrefix(oldpBitStrm.buf, old2pBitStrm.buf, 0, currentByte - 1))

         if (cb > 0) {
            arrayUpdatedAtPrefixLemma(oldpBitStrm.buf, currentByte, buf(currentByte))
            arrayUpdatedAtPrefixLemma(old2pBitStrm.buf, currentByte, buf(currentByte))
            arrayPrefixTransitive(
               oldpBitStrm.buf,
               old2pBitStrm.buf,
               buf,0, currentByte - 1, currentByte
            )
            check(arrayPrefix(
               oldpBitStrm.buf,
               buf,
               0,
               oldpBitStrm.currentByte
            ))
         } else {
            check(arrayPrefix(
               oldpBitStrm.buf,
               buf,0,
               oldpBitStrm.currentByte
            ))
         }
      }
   }.ensuring { _ =>
      val w1 = old(this)
      val w2 = this
      w2.bitIndex() == w1.bitIndex() + 8 &&& isValidPair(w1, w2) &&& {
         val (r1, r2) = reader(w1, w2)
         val (r2Got, vGot) = BitStream_ReadBytePure(r1)
         ((!negate && vGot.get == value) || (negate && vGot.get == ~value)) && r2Got == r2
      } &&& BitStream.invariant(this)
   }

   def appendByte0(v: UByte): Boolean = {
      require(BitStream.validate_offset_bytes(this, 1))
      val cb: UByte = currentBit.toByte
      val ncb: UByte = (8 - cb).toByte

      var mask = ~masksb(ncb)

      buf(currentByte) = (buf(currentByte) & mask).toByte
      buf(currentByte) = (buf(currentByte) | (v >>>> cb)).toByte
      currentByte += 1

      if cb > 0 then
         if currentByte >= buf.length then
            return false
         mask = ~mask
         ghostExpr {
            ensureInvariant()
         }
         buf(currentByte) = (buf(currentByte) & mask).toByte
         buf(currentByte) = (buf(currentByte) | (v <<<< ncb)).toByte

      true
   }.ensuring(_ => BitStream.invariant(this))

} // BitStream class
