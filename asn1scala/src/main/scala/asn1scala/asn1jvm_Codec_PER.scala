package asn1scala

import stainless.lang.StaticChecks._
import stainless.annotation._

/**
 * Get an instance of a PER coded bitstream
 * @param count of elements in underlying buffer
 * @return PER coded bitstream
 */
def initPERCodec(count: Int): PER = {
   PER(Codec(BitStream(Array.fill(count)(0))))
}

case class PER private [asn1scala](base: Codec) {
   import BitStream.*
   export base.{isPrefixOf => _, withMovedBitIndex => _, withMovedByteIndex => _, *}

   @ghost @pure @inline
   def resetAt(other: PER): PER = {
      require(bitStream.buf.length == other.base.bitStream.buf.length)
      PER(Codec(bitStream.resetAt(other.base.bitStream)))
   }

   @ghost @pure @inline
   def withMovedByteIndex(diffInBytes: Int): PER = {
      require(moveByteIndexPrecond(bitStream, diffInBytes))
      PER(Codec(bitStream.withMovedByteIndex(diffInBytes)))
   }

   @ghost @pure @inline
   def withMovedBitIndex(diffInBits: Int): PER = {
      require(moveBitIndexPrecond(bitStream, diffInBits))
      PER(Codec(bitStream.withMovedBitIndex(diffInBits)))
   }

   @pure @inline
   def isPrefixOf(per2: PER): Boolean = bitStream.isPrefixOf(per2.base.bitStream)
}
