package asn1scala

import stainless.*
import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}
import stainless.collection.*
import stainless.annotation.*
import stainless.proof.*
import stainless.math.*
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
}

def GetNumberOfBitsInUpperBytesAndDecreaseValToLastByte(v: UInt): (UInt, Int) = {
   if v >>> 8 == 0 then
      (v, 0)
   else if v >>> 16 == 0 then
      (v >>> 8, 8)
   else if v >>> 24 == 0 then
      (v >>> 16, 16)
   else
      (v >>> 24, 24)
}.ensuring((v, n) => v >= 0 &&& v <= 0xFF &&& n >= 0 &&& n <= 24)

def GetNumberOfBitsInLastByteRec (vVal: UInt, n: UInt): Int = {
   require(vVal >= 0 && vVal <= 0xFF)
   require(n >= 0 && n <= 8)
   require(1<<(8-n) > vVal)
   decreases(8-n)

   if(vVal == 0) then
      n
   else
      GetNumberOfBitsInLastByteRec(vVal >>> 1, n+1)
}

def GetNumberOfBitsForNonNegativeInteger32(vVal: UInt): Int = {
   val (v, n) = GetNumberOfBitsInUpperBytesAndDecreaseValToLastByte(vVal)
   n + GetNumberOfBitsInLastByteRec(v, 0)
}

def GetNumberOfBitsForNonNegativeInteger(v: ULong): Int = {
   if v >>> 32 == 0 then
      GetNumberOfBitsForNonNegativeInteger32(v.cutToInt)
   else
      val h = (v >>> 32).cutToInt
      32 + GetNumberOfBitsForNonNegativeInteger32(h)
}.ensuring(n => n >= 0 && n <= 64)

def GetLengthForEncodingUnsigned(v: ULong): Int = {
   max((GetNumberOfBitsForNonNegativeInteger(v) + NO_OF_BITS_IN_BYTE - 1) / NO_OF_BITS_IN_BYTE, 1) // even the number 0 needs 1 byte
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

sealed trait OptionMut[@mutable A]
case class NoneMut[@mutable A]() extends OptionMut[A]
case class SomeMut[@mutable A](v: A) extends OptionMut[A]
