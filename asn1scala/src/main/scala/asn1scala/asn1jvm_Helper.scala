package asn1scala

import stainless.*
import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}
import stainless.collection.*
import stainless.annotation.*
import stainless.proof.*
import stainless.math.*
import StaticChecks.*

// all bits of the integer
val MASK_BYTE       = 0xFF
val MASK_BYTE_L     = 0xFFL
val MASK_SHORT_L    = 0xFF_FFL
val MASK_INT_L      = 0xFF_FF_FF_FFL

// MSBs (neg bits of the integer)
val MASK_MSB_BYTE   = 0x80L
val MASK_MSB_INT    = 0x80_00_00_00L

// pos bits of the integer
val MASK_POS_BYTE   = 0x7FL
val MASK_POS_INT    = 0x7F_FF_FF_FFL

/*
* Meths to upcast unsigned integer data types on the JVM
*/
extension (ub: UByte) {
   def unsignedToLong: Long = ub & MASK_BYTE_L
   def unsignedToInt: Int = ub.toInt & MASK_BYTE
}

extension (us: UShort) {
   def unsignedToLong: Long = us & MASK_SHORT_L
}

extension (ui: UInt) {
   def unsignedToLong: Long = ui & MASK_INT_L
}

extension (i: Int) {
   def toUnsignedByte: UByte = {

      var iVal = i & MASK_BYTE

      if(iVal == MASK_MSB_BYTE)
         (-MASK_MSB_BYTE).toByte
      else if ((i & MASK_MSB_BYTE) == MASK_MSB_BYTE)
         ((i & MASK_POS_BYTE) - MASK_MSB_BYTE).toByte
      else
         i.toByte
   }
}

extension (l: Long) {
   def toUnsignedInt: UInt = {
      require(l >= 0 && l <= MASK_INT_L)

      if(l == MASK_MSB_INT)
         (-MASK_MSB_INT).toInt
      else if ((l & MASK_MSB_INT) == MASK_MSB_INT)
         ((l & MASK_POS_INT) - MASK_MSB_INT).toInt
      else
         l.toInt
   }
}

extension (b: Byte) {
   def >>>>(i: Int): Byte = {
      require(i >= 0 && i <= 8)
      ((b.toInt & MASK_BYTE) >>> i).toUnsignedByte
   }

   def <<<<(i: Int): Byte = {
      require(i >= 0 && i <= 8)
      ((b.toInt << i) & MASK_BYTE).toUnsignedByte
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
      GetNumberOfBitsForNonNegativeInteger32(v.toUnsignedInt)
   else
      val h = (v >>> 32).toUnsignedInt
      32 + GetNumberOfBitsForNonNegativeInteger32(h)
}.ensuring(n => n >= 0 && n <= 64)

def GetLengthInBytesOfUInt (v: ULong): Int = {
   GetLengthInBytesOfSInt(v) // just call signed, is signed anyway
}.ensuring(n => n > 0 && n <= NO_OF_BYTES_IN_JVM_LONG)

def GetLengthInBytesOfSInt (v: Long): Int = {
   max((GetNumberOfBitsForNonNegativeInteger(v) + NO_OF_BITS_IN_BYTE - 1) / NO_OF_BITS_IN_BYTE, 1) // even the number 0 needs 1 byte
}.ensuring(n => n > 0 && n <= NO_OF_BYTES_IN_JVM_LONG)

/**
Binary encoding will be used
REAL = M*B^E
where
M = S*N*2^F

ENCODING is done within three parts
part 1 is 1 byte header
part 2 is 1 or more byte for exponent
part 3 is 3 or more byte for mantissa (N)

First byte
S :0-->+, S:1-->-1
Base will be always be 2 (implied by 6th and 5th bit which are zero)
ab: F    (0..3)
cd:00 --> 1 byte for exponent as 2's complement
cd:01 --> 2 byte for exponent as 2's complement
cd:10 --> 3 byte for exponent as 2's complement
cd:11 --> 1 byte for encoding the length of the exponent, then the exponent

8 7 6 5 4 3 2 1
+-+-+-+-+-+-+-+-+
|1|S|0|0|a|b|c|d|
+-+-+-+-+-+-+-+-+
 **/

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
   &&& m >= 0 &&& m <= MantissaBitMask)

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
