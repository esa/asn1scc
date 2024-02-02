package asn1scala

import stainless.*
import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}
import stainless.collection.*
import stainless.annotation.*
import stainless.proof.*
import stainless.math.{wrapping => wrappingExpr, *}
import StaticChecks.*
import scala.annotation.targetName

// all bits of the integer
val MASK_BYTE       = 0xFF
val MASK_BYTE_L     = 0xFFL
val MASK_SHORT_L    = 0xFF_FFL
val MASK_INT_L      = 0xFF_FF_FF_FFL

// MSBs (neg bits of the integer)
val MASK_MSB_BYTE   = 0x80L
val MASK_MSB_SHORT  = 0x80_00L
val MASK_MSB_INT    = 0x80_00_00_00L

// pos bits of the integer
val MASK_POS_BYTE   = 0x7FL
val MASK_POS_SHORT  = 0x7F_FFL
val MASK_POS_INT    = 0x7F_FF_FF_FFL

val MASK_B: Array[Byte] = Array(
   0x00, //   0 / 0000 0000 / x00
   0x01, //   1 / 0000 0001 / x01
   0x03, //   3 / 0000 0011 / x03
   0x07, //   7 / 0000 0111 / x07
   0x0F, //  15 / 0000 1111 / x0F
   0x1F, //  31 / 0001 1111 / x1F
   0x3F, //  63 / 0011 1111 / x3F
   0x7F, // 127 / 0111 1111 / x7F
   -0x1, //  -1 / 1111 1111 / xFF
)

val MASK_C: Array[Byte] = Array(
   0x00,  //  / 0000 0000 /
   -0x80,  //  / 1000 0000 /
   -0x40,  //  / 1100 0000 /
   -0x20,  //  / 1110 0000 /
   -0x10,  //  / 1111 0000 /
   -0x08,  //  / 1111 1000 /
   -0x04,  //  / 1111 1100 /
   -0x02,  //  / 1111 1110 /
)

/*
* Meths to upcast unsigned integer data types on the JVM
*/
extension (ubL: UByte) {
   def unsignedToLong: Long = ubL.toLong & MASK_BYTE_L
   def unsignedToInt: Int = ubL.toInt & MASK_BYTE

   @targetName("unsigned right shift on Bytes")
   def >>>>(i: Int): UByte = {
      require(i >= 0 && i <= NO_OF_BITS_IN_BYTE)
      ((ubL.toInt & MASK_BYTE) >>> i).cutToByte
   }

   @targetName("left shift on Bytes")
   def <<<<(i: Int): UByte = {
      require(i >= 0 && i <= NO_OF_BITS_IN_BYTE)
      ((ubL.toInt << i) & MASK_BYTE).cutToByte
   }

   @targetName("binary OR on Bytes")
   def |||(ubR: Byte): UByte = {
      (ubL.toInt | ubR.toInt).cutToByte
   }
}

extension (us: UShort) {
   def unsignedToLong: Long = us & MASK_SHORT_L
}

extension (ui: UInt) {
   def unsignedToLong: Long = ui & MASK_INT_L
}

extension (i: Int) {
   def toUnsignedByte: UByte = {

      if((i & MASK_BYTE) == MASK_MSB_BYTE)
         (-MASK_MSB_BYTE).toByte
      else if ((i & MASK_MSB_BYTE) == MASK_MSB_BYTE)
         ((i & MASK_POS_BYTE) - MASK_MSB_BYTE).toByte
      else
         (i & MASK_BYTE).toByte
   }
}

extension (l: Long) {
   def cutToInt: UInt = {
      if(l == MASK_MSB_INT)
         (-MASK_MSB_INT).toInt
      else if ((l & MASK_MSB_INT) == MASK_MSB_INT)
         ((l & MASK_POS_INT) - MASK_MSB_INT).toInt
      else
         (l & MASK_INT_L).toInt
   }

   def cutToShort: UShort = {
      if (l == MASK_MSB_SHORT)
         (-MASK_MSB_SHORT).toShort
      else if ((l & MASK_MSB_SHORT) == MASK_MSB_SHORT)
         ((l & MASK_POS_SHORT) - MASK_MSB_SHORT).toShort
      else
         (l & MASK_SHORT_L).toShort
   }

   def cutToByte: UByte = {
      if ((l & MASK_BYTE) == MASK_MSB_BYTE)
         (-MASK_MSB_BYTE).toByte
      else if ((l & MASK_MSB_BYTE) == MASK_MSB_BYTE)
         ((l & MASK_POS_BYTE) - MASK_MSB_BYTE).toByte
      else
         (l & MASK_BYTE_L).toByte
   }

   @extern
   def toByteArray: Array[Byte] = {
      scala.math.BigInt(l).toByteArray
   }

   // less than & equal for unsigned numbers
   @inline
   def lteUnsigned(r: ULong): Boolean = {
      // This corresponds to java.lang.Long.compareUnsigned
      wrappingExpr { l + Long.MinValue <= r + Long.MinValue }
   }

   @inline
   def u_<=(r: ULong): Boolean = l.lteUnsigned(r)
}

extension [T](arr: Array[T]) {
   def indexOf(elem: T): Int = {
      def rec(i: Int): Int = {
         require(0 <= i && i <= arr.length)
         decreases(arr.length - i)
         if (i == arr.length) -1
         else if (arr(i) == elem) i
         else rec(i + 1)
      }
      rec(0)
   }

   def sameElements(other: Array[T]): Boolean = arraySameElements(arr, other)
}

// TODO: FIXME: To get around aliasing restriction, ideally we should do things differently
@extern @pure
def freshCopyHack[@mutable T](t: T): T = t.ensuring(_ == t)

/**
 * Get number of bits needed to represent the value v
 *
 * Example:
 *                                4 bits needed
 *                                v
 *    v = 12d = 0b0000'0...0'0000'1100b
 *
 * @param v value that should get serialised
 * @return number of bits needed for serialisation
 */
def GetBitCountUnsigned(v: ULong): Int = {
   if v < 0 then
      return NO_OF_BITS_IN_LONG

   if v == 0 then
      return 0

   var i = 0
   var l = v
   (while i < NO_OF_BITS_IN_LONG - 1 && l != 0 do
      decreases(NO_OF_BITS_IN_LONG - i)
      l >>>= 1
      i += 1
   ).invariant(
      l >= 0 &&
      i >= 0 && i < NO_OF_BITS_IN_LONG &&            // i must be in range [0,63]
      (i == 0 && Long.MaxValue >= l ||
         1L << ((NO_OF_BITS_IN_LONG - 1) - i) >= l)) // i == 0 v (∀i : 0 < i < 64: (1 << i) > l)

   i
}.ensuring(x =>
   x >= 0 && x <= NO_OF_BITS_IN_LONG && (x == NO_OF_BITS_IN_LONG && v < 0 || v >>> x == 0))

/**
 * Get number of bits needed to encode the singed value v
 *
 * Example:
 *                              5 bit (include sign bit)
 *                              v'vvvv
 *    v = 12d = 0b0000'0...0'0000'1100b
 *
 * @param v value to encode
 * @return number of bits needed
 */
def GetBitCountSigned(v: Long): Int = {
   val MSB = (v & 1L << NO_OF_BITS_IN_LONG - 1) != 0

   var i = NO_OF_BITS_IN_LONG
   (while i >= 2 && ((v & (1L << (i-2))) != 0) == MSB do
      decreases(i)
      i -= 1
   ).invariant(i >= 1 && i <= NO_OF_BITS_IN_LONG)

   i

}.ensuring(x =>
   x >= 0 && x <= NO_OF_BITS_IN_LONG)

/**
 * Get number of bytes needed to represent the value v
 *
 * Remarks: Calls GetBitCountUnsigned,
 * rounds to next whole byte
 *
 * Example:
 *    v = 12d = 0b0000'0...0'0000'1100b
 *    -> 4Bit -> 1Byte has sufficient space
 *
 * @param v value that needs its byte count evaluated
 * @return number of full bytes needed for serialisation
 */
def GetLengthForEncodingUnsigned(v: ULong): Int = {
   max((GetBitCountUnsigned(v) + NO_OF_BITS_IN_BYTE - 1) / NO_OF_BITS_IN_BYTE, 1) // even the number 0 needs 1 byte
}.ensuring(n => n > 0 && n <= NO_OF_BYTES_IN_JVM_LONG)

/**
 * Get number of bytes needed to encode the
 * positive number v according to PER rules (8.3)
 *
 * Example:
 *    v = 12d = 0b0000'0...0'0000'1100b
 *    According to 8.3.3 Leading zeros do not have to be
 *    serialised, hence just send a single octet (0000'1100)
 *
 * @param v value that should get serialised
 * @return number of bytes needed for serialisation
 */
def GetBytesNeededForPositiveNumber(v: Long): Int = {
   require(v >= 0)

   v match
      case x if x < (1L << 7) => 1
      case x if x < (1L << 15) => 2
      case x if x < (1L << 23) => 3
      case x if x < (1L << 31) => 4
      case x if x < (1L << 39) => 5
      case x if x < (1L << 47) => 6
      case x if x < (1L << 55) => 7
      case _ => 8

}.ensuring(n =>
   n > 0 && n <= NO_OF_BYTES_IN_JVM_LONG &&&
   n == NO_OF_BYTES_IN_JVM_LONG || (1L << (NO_OF_BITS_IN_BYTE * n - 1) > v)
)

/**
 * Get number of bytes needed to encode the
 * negative number v according to PER rules (8.3)
 *
 * Example:
 *    v = -1d = 0b1111'1...1'1111'1111b
 *    According to 8.3.3 Leading ones do not have to be
 *    serialised, hence just send a single octet (1111'1111)
 *
 * @param v value that should get serialised
 * @return number of bytes needed for serialisation
 */
def GetBytesNeededForNegativeNumber(v: Long): Int = {
   require(v < 0)

   v match
      case x if x >= 0xFFFF_FFFF_FFFF_FF80L => 1
      case x if x >= 0xFFFF_FFFF_FFFF_8000L => 2
      case x if x >= 0xFFFF_FFFF_FF80_0000L => 3
      case x if x >= 0xFFFF_FFFF_8000_0000L => 4
      case x if x >= 0xFFFF_FF80_0000_0000L => 5
      case x if x >= 0xFFFF_8000_0000_0000L => 6
      case x if x >= 0xFF80_0000_0000_0000L => 7
      case _ => NO_OF_BYTES_IN_JVM_LONG // 8

}.ensuring(
   n => {
      val pre = n > 0 && n <= NO_OF_BYTES_IN_JVM_LONG
      var i = n * NO_OF_BITS_IN_BYTE - 1
      var allOnes = true

      // (∀i : 8n < i < 64: v[i] == 1)
      (while i < NO_OF_BITS_IN_LONG do
         decreases(NO_OF_BITS_IN_LONG - i)
         allOnes &= (v & (1L << i)) != 0
         i += 1
      ).invariant(i >= (n * NO_OF_BITS_IN_BYTE - 1) && i <= NO_OF_BITS_IN_LONG)

      pre &&& allOnes
   })

/**
 * Get number of bytes needed to encode the
 * number v according to the PER rules (8.3)
 *
 * @param v signed value that should get serialised
 * @return number of bytes needed for serialisation
 */
def GetLengthForEncodingSigned(v: Long): Int = {
   if v >= 0 then
      GetBytesNeededForPositiveNumber(v)
   else
      GetBytesNeededForNegativeNumber(v)
}.ensuring(n => n > 0 && n <= NO_OF_BYTES_IN_JVM_LONG)

def CalculateMantissaAndExponent(doubleAsLong64: Long): (UInt, ULong) = {
   require({
      val rawExp = (doubleAsLong64 & ExpoBitMask) >>> DoubleNoOfMantissaBits
      rawExp >= 0 &&& rawExp <= ((1 << 11) - 2) // 2046, 2047 is the infinity case - never end up here with infinity
   })

   val exponent: UInt = ((doubleAsLong64 & ExpoBitMask) >>> DoubleNoOfMantissaBits).toInt - DoubleBias.toInt - DoubleNoOfMantissaBits.toInt
   var mantissa: ULong = doubleAsLong64 & MantissaBitMask
   mantissa = mantissa | MantissaExtraBit

   (exponent, mantissa)

}.ensuring((e, m) => e >= (-DoubleBias - DoubleNoOfMantissaBits) &&& e <= (DoubleBias - DoubleNoOfMantissaBits)
   &&& m >= 0 &&& m <= (MantissaBitMask | MantissaExtraBit))

def GetDoubleBitStringByMantissaAndExp(mantissa: ULong, exponentVal: Int): Long = {
   ((exponentVal + DoubleBias + DoubleNoOfMantissaBits) << DoubleNoOfMantissaBits) | (mantissa & MantissaBitMask)
}

/**
Helper function for REAL encoding

Negative Ints always need 4 bytes of space, the ASN.1 standard compacts those numbers down
to 8, 16 or 24 bits depending on the leading bytes full of 1s.

Example:
-4 in Int: 0b1111_..._1111_1100
--> compacted to 0b1111_1100

The ASN.1 header holds the detail on how to interpret this number
 **/
def RemoveLeadingFFBytesIfNegative(v: Int): Int = {
   if v >= 0 then
      v
   else if v >= Byte.MinValue then
      v & 0xFF
   else if v >= Short.MinValue then
      v & 0xFF_FF
   else if v >= -8_388_608 then
      v & 0xFF_FF_FF
   else
      v
}

sealed trait OptionMut[@mutable A] {
   def isDefined: Boolean = this match {
      case SomeMut(_) => true
      case NoneMut() => false
   }
   def get: A = {
      require(isDefined)
      (this: @unchecked) match {
         case SomeMut(a) => a
      }
   }
}
case class NoneMut[@mutable A]() extends OptionMut[A]
case class SomeMut[@mutable A](v: A) extends OptionMut[A]

sealed trait EitherMut[@mutable A, @mutable B]
case class LeftMut[@mutable A, @mutable B](a: A) extends EitherMut[A, B]
case class RightMut[@mutable A, @mutable B](b: B) extends EitherMut[A, B]
