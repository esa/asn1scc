package asn1scala

import stainless.*
import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}
import stainless.collection.*
import stainless.annotation.*
import stainless.proof.*
import stainless.math.{wrapping => wrappingExpr, *}
import StaticChecks.*
import scala.annotation.targetName

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
   -0x01,  //  / 1111 1111 /
)

val BitAccessMasks: Array[Byte] = Array(
   -0x80, // -128 / 1000 0000 / x80
   0x40, //   64 / 0100 0000 / x40
   0x20, //   32 / 0010 0000 / x20
   0x10, //   16 / 0001 0000 / x10
   0x08, //    8 / 0000 1000 / x08
   0x04, //    4 / 0000 0100 / x04
   0x02, //    2 / 0000 0010 / x02
   0x01, //    1 / 0000 0001 / x01
)

extension [T](arr: Array[T]) {
   def indexOf(elem: T): Int = {
      def rec(i: Int): Int = {
         require(0 <= i && i <= arr.length)
         decreases(arr.length - i)
         if (i == arr.length) -1
         else if (arr(i) == elem) i
         else rec(i + 1)
      }.ensuring(res => -1 <= res && res < arr.length)
      rec(0)
   }.ensuring(res => -1 <= res && res < arr.length)

   def indexOfOrLength(elem: T): Int = {
      val ix = indexOf(elem)
      if (ix == -1) arr.length else ix
   }.ensuring(res => 0 <= res && res <= arr.length)

   def sameElements(other: Array[T]): Boolean = arraySameElements(arr, other)
}

extension [T](vec: Vector[T]) {
   def sameElements(other: Vector[T]): Boolean = vecSameElements(vec, other)
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
def GetBitCountUnsigned(vv: ULong): Int = {
   val v = vv.toRaw

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
   x >= 0 && x <= NO_OF_BITS_IN_LONG && (x == NO_OF_BITS_IN_LONG && vv.toRaw < 0 || vv.toRaw >>> x == 0))

/**
 * Get number of bits needed to encode the signed value v
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
}.ensuring { nBits =>
   0 <= nBits && nBits <= NO_OF_BITS_IN_LONG &&
   ((v < 0) ==>
      ((onesMSBLong(NO_OF_BITS_IN_LONG - nBits) | (v & onesLSBLong(nBits))) == v &&
      ((v & onesLSBLong(nBits)) & (1L << (nBits - 1))) != 0L)) &&
   ((v >= 0) ==> ((v & onesLSBLong(nBits)) == v && (v & (1L << (nBits - 1))) == 0L))
}

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

   val exponent = ((doubleAsLong64 & ExpoBitMask) >>> DoubleNoOfMantissaBits).toInt - DoubleBias.toInt - DoubleNoOfMantissaBits.toInt
   var mantissa = doubleAsLong64 & MantissaBitMask
   mantissa = mantissa | MantissaExtraBit

   (UInt.fromRaw(exponent), ULong.fromRaw(mantissa))

}//.ensuring((e, m) => (-DoubleBias - DoubleNoOfMantissaBits).toInt.toRawUInt <= e &&& e <= (DoubleBias - DoubleNoOfMantissaBits).toInt.toRawUInt
   //&&& 0.toRawULong <= m &&& m <= (MantissaBitMask | MantissaExtraBit).toRawULong)

def GetDoubleBitStringByMantissaAndExp(mantissa: ULong, exponentVal: Int): Long = {
   ((exponentVal + DoubleBias + DoubleNoOfMantissaBits) << DoubleNoOfMantissaBits) | (mantissa.toRaw & MantissaBitMask)
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

sealed trait EitherMut[@mutable A, @mutable B] {
   def isRight: Boolean = this match {
      case RightMut(_) => true
      case LeftMut(_) => false
   }
   def isLeft: Boolean = !isRight
}
case class LeftMut[@mutable A, @mutable B](a: A) extends EitherMut[A, B]
case class RightMut[@mutable A, @mutable B](b: B) extends EitherMut[A, B]
