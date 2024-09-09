package asn1scala

import stainless.lang.{None => None, Option => Option, ghost => ghostExpr, _}
import stainless.annotation._
import stainless.math.{wrapping => wrappingExpr, *}
import StaticChecks.*
import scala.annotation.targetName
import scala.language.implicitConversions

// type used in ErrorCases
type ErrorCode = Int

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

// Unsigned datatypes that have no native JVM support
opaque type UByte = Byte
object UByte {
    @inline def fromRaw(u: Byte): UByte = u
    @inline @pure def fromArrayRaws(arr: Array[Byte]): Array[UByte] = arr
    @inline @pure def fromVectorRaws(arr: Vector[Byte]): Vector[UByte] = arr
}
extension (l: UByte) {
    @inline def toRaw: Byte = l
    @inline def toULong: ULong = l
    @inline def <=(r: UByte): Boolean = wrappingExpr { l + Byte.MinValue <= r + Byte.MinValue }

    def unsignedToLong: Long = l.toLong & MASK_BYTE_L
    def unsignedToInt: Int = l.toInt & MASK_BYTE

    @targetName("unsigned right shift on Bytes")
    def >>>>(i: Int): UByte = {
        require(i >= 0 && i <= NO_OF_BITS_IN_BYTE)
        ((l.toInt & MASK_BYTE) >>> i).cutToByte
    }

    @targetName("left shift on Bytes")
    def <<<<(i: Int): UByte = {
        require(i >= 0 && i <= NO_OF_BITS_IN_BYTE)
        ((l.toInt << i) & MASK_BYTE).cutToByte
    }

    @targetName("binary OR on Bytes")
    def |||(ubR: Byte): UByte = {
        (l.toInt | ubR.toInt).cutToByte
    }
}
extension (arr: Array[UByte]) {
    @inline def toArrayRaws: Array[Byte] = arr
}

extension (vec: Vector[UByte]) {
    @inline def toVectorRaws: Vector[Byte] = vec
}

opaque type UShort = Short

object UShort {
    @inline def fromRaw(u: Short): UShort = u
}
extension (l: UShort) {
    @inline def toRaw: Short = l
    @inline def <=(r: UShort): Boolean = wrappingExpr { l + Short.MinValue <= r + Short.MinValue }
    @inline def unsignedToLong: Long = l & MASK_SHORT_L
}

opaque type UInt = Int
object UInt {
    @inline def fromRaw(u: Int): UInt = u
}
extension (l: UInt) {
    @inline def toRaw: Int = l
    @inline def <=(r: UInt): Boolean = wrappingExpr { l + Int.MinValue <= r + Int.MinValue }
    @inline def <(r: UInt): Boolean = wrappingExpr { l + Int.MinValue < r + Int.MinValue }
    @inline def >(r: UInt): Boolean = wrappingExpr { l + Int.MinValue > r + Int.MinValue }
    @inline def >=(r: UInt): Boolean = wrappingExpr { l + Int.MinValue >= r + Int.MinValue }
    @inline def unsignedToLong: Long = l & MASK_INT_L
    @inline def toULong: ULong = l
}

opaque type ULong = Long
object ULong {
    @inline def fromRaw(u: Long): ULong = u
}
extension (l: ULong) {
    @inline def toRaw: Long = l
    @inline def toUByte: UByte = wrappingExpr { l.toByte }
    @inline def toUShort: UShort = wrappingExpr { l.toShort }
    @inline def toUInt: UInt = wrappingExpr { l.toInt }
    @inline def <=(r: ULong): Boolean = wrappingExpr { l + Long.MinValue <= r + Long.MinValue }
    @inline def <(r: ULong): Boolean = wrappingExpr { l + Long.MinValue < r + Long.MinValue }
    @inline def >(r: ULong): Boolean = wrappingExpr { l + Long.MinValue > r + Long.MinValue }
    @inline def >=(r: ULong): Boolean = wrappingExpr { l + Long.MinValue >= r + Long.MinValue }
    @inline def +(r: ULong): ULong = wrappingExpr { l + r }
    @inline def -(r: ULong): ULong = wrappingExpr { l - r }
}

extension (b: Byte) {
    @inline def toRawUByte: UByte = UByte.fromRaw(b)
}
extension (i: Int) {
    @inline def toRawUInt: UInt = i

    @inline def toRawUByte: UByte = wrappingExpr { i.toByte.toRawUByte }

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
    @inline def toRawULong: ULong = l

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

@extern
type RealNoRTL = Float
type BooleanNoRTL = Boolean
type ASCIIChar = UByte
type NullType = Byte

// TODO
type LongNoRTL = Long
type ULongNoRTL = ULong

// Floating Point Types
@extern
type asn1Real = Double

val NO_OF_BITS_IN_BYTE = 8
val NO_OF_BITS_IN_SHORT = 16
val NO_OF_BITS_IN_INT = 32
val NO_OF_BITS_IN_LONG = 64
val NO_OF_BYTES_IN_JVM_SHORT = 2
val NO_OF_BYTES_IN_JVM_INT = 4
val NO_OF_BYTES_IN_JVM_LONG = 8
val NO_OF_BYTES_IN_JVM_FLOAT = 4
val NO_OF_BYTES_IN_JVM_DOUBLE = 8
val OBJECT_IDENTIFIER_MAX_LENGTH = 20

val NOT_INITIALIZED_ERR_CODE = 1337
val ERR_INVALID_ENUM_VALUE = 2805

// Floating Point Mask & Variables
val ExpoBitMask = 0x7FF0_0000_0000_0000L
val MantissaBitMask = 0x000F_FFFF_FFFF_FFFFL
val MantissaExtraBit = 0x0010_0000_0000_0000L // hidden bit
val SignBitMask = 0x8000_0000_0000_0000L
val InverseSignBitMask = 0x7FFF_FFFF_FFFF_FFFFL

val DoublePosInfBitString = ExpoBitMask
val DoubleNegInfBitString = 0xFFF0_0000_0000_0000L
val DoublePosZeroBitString = 0x0000_0000_0000_0000L
val DoubleNegZeroBitString = SignBitMask
val DoubleNotANumber = 0x7FF8_0000_0000_0000L // definied in java.lang.Double.NaN
val DoubleMaxLengthOfSentBytes = 10 // mantissa max 7, exp max 2, header 1

val NoOfSignBit = 1 // double & float
val DoubleNoOfExponentBits = 11L
val DoubleNoOfMantissaBits = 52L
val DoubleBias = (1L << 10) - 1 // 1023


val ber_aux: Array[Long] = Array(
    0xFFL,
    0xFF00L,
    0xFF0000L,
    0xFF000000L,
    0xFF00000000L,
    0xFF0000000000L,
    0xFF000000000000L,
    0xFF00000000000000L
)

// TODO: check types and if neccesary as we don't have unsigned types
def int2uint(v: Long): ULong = {
    var ret: Long = 0L
    if v < 0 then
        ret = wrappingExpr(-v - 1)
        ret = wrappingExpr(~ret)
    else
        ret = v

    ULong.fromRaw(ret)
}

def onesLSBLong(nBits: Int): Long = {
    require(0 <= nBits && nBits <= 64)
    -1L >>> (64 - nBits)
}

def bitLSBLong(bit: Boolean, nBits: Int): Long = {
    require(0 <= nBits && nBits <= 64)
    if bit then onesLSBLong(nBits) else 0L
}

def onesMSBLong(nBits: Int): Long = {
    require(0 <= nBits && nBits <= 64)
    // Note: on the JVM, -1L << 64 == -1L, not 0L as sane persons would expect
    if (nBits == 0) 0L else -1L << (64 - nBits)
}

def bitMSBLong(bit: Boolean, nBits: Int): Long = {
    require(0 <= nBits && nBits <= 64)
    if bit then onesMSBLong(nBits) else 0L
}

def alignedToN(alignment: Long, bits: Long): Long = {
    require(2L <= alignment && alignment <= 64L && 0L <= bits && bits <= Long.MaxValue - alignment)
    val rem = bits % alignment
    if (rem != 0L) bits + (alignment - rem)
    else bits
}

def alignedSizeToN(alignment: Long, offset: Long, bits: Long): Long = {
    require(2L <= alignment && alignment <= 64L && 0L <= bits && bits <= Long.MaxValue - alignment)
    require(offset >= 0L)
    val rem = offset % alignment
    if (rem != 0L) bits + (alignment - rem)
    else bits
}

def alignedToByte(bits: Long): Long = {
    require(0L <= bits && bits <= Long.MaxValue - 8L)
    alignedToN(8L, bits)
}.ensuring(res => res % 8L == 0L && bits <= res && res <= bits + 7L)

def alignedToWord(bits: Long): Long = {
    require(0L <= bits && bits <= Long.MaxValue - 16L)
    alignedToN(16L, bits)
}.ensuring(res => res % 16L == 0L && bits <= res && res <= bits + 15L)

def alignedToDWord(bits: Long): Long = {
    require(0L <= bits && bits <= Long.MaxValue - 32L)
    alignedToN(32L, bits)
}.ensuring(res => res % 32L == 0L && bits <= res && res <= bits + 31L)

def alignedSizeToByte(bits: Long, offset: Long): Long = {
    require(0L <= bits && bits <= Long.MaxValue - 8L)
    require(offset >= 0L)
    alignedSizeToN(8L, offset, bits)
}.ensuring(res => bits <= res && res <= bits + 7L)

def alignedSizeToWord(bits: Long, offset: Long): Long = {
    require(0L <= bits && bits <= Long.MaxValue - 16L)
    require(offset >= 0L)
    alignedSizeToN(16L, offset, bits)
}.ensuring(res => bits <= res && res <= bits + 15L)

def alignedSizeToDWord(bits: Long, offset: Long): Long = {
    require(0L <= bits && bits <= Long.MaxValue - 32L)
    require(offset >= 0L)
    alignedSizeToN(32L, offset, bits)
}.ensuring(res => bits <= res && res <= bits + 31L)

def uint2intWhile(v: ULong, uintSizeInBytes: Int): Long = {
    require(uintSizeInBytes >= 1 && uintSizeInBytes <= 9)

    var vv = v.toRaw
    val tmp: ULong = 0x80
    val bIsNegative: Boolean = (vv & (tmp << ((uintSizeInBytes - 1) * 8))) > 0

    if !bIsNegative then
        return v

    var i: Int = NO_OF_BYTES_IN_JVM_LONG-1 // 7
    (while i >= uintSizeInBytes do
        decreases(i)
        vv |= ber_aux(i)
        i -= 1
      ).invariant(i <= NO_OF_BYTES_IN_JVM_LONG-1 && i >= uintSizeInBytes - 1)
    -(~vv) - 1
}

/**
  * Version of uint2int that unfolds completely the loop, to help verification
  *
  * @param v
  * @param uintSizeInBytes
  */
def uint2int(v: ULong, uintSizeInBytes: Int): Long = {
    require(uintSizeInBytes >= 1 && uintSizeInBytes <= 9)

    var vv = v.toRaw
    val tmp: ULong = 0x80
    val bIsNegative: Boolean = (vv & (tmp << ((uintSizeInBytes - 1) * 8))) > 0

    if !bIsNegative then
        return v

    if(uintSizeInBytes <= 7) then vv |= ber_aux(7)
    if(uintSizeInBytes <= 6) then vv |= ber_aux(6)
    if(uintSizeInBytes <= 5) then vv |= ber_aux(5)
    if(uintSizeInBytes <= 4) then vv |= ber_aux(4)
    if(uintSizeInBytes <= 3) then vv |= ber_aux(3)
    if(uintSizeInBytes <= 2) then vv |= ber_aux(2)
    if(uintSizeInBytes <= 1) then vv |= ber_aux(1)

    -(~vv) - 1
}



def GetCharIndex(ch: UByte, charSet: Array[UByte]): Int =
{
    var i: Int = 0
    // TODO what is this? why is 0 the default return? what is the difference between key found in 0 and default?
    var ret: Int = 0

    (while i < charSet.length && ret == 0 do
        decreases(charSet.length - i)
        if ch == charSet(i) then
            ret = i
        i += 1
      ).invariant(i >= 0 &&& i <= charSet.length && ret < charSet.length && ret >= 0)
    ret
}.ensuring(res => charSet.length == 0 || res >= 0 && res < charSet.length)

def NullType_Initialize(): NullType = {
    0
}

@extern @pure
def asn1Real_Initialize(): asn1Real = {
    0.0
}

case class ByteStream (
    var buf: Array[Byte], // UByte
    var currentByte: Int,
    var EncodeWhiteSpace: Boolean
) {
    require(currentByte >= 0 && currentByte < buf.length)
}

case class Token (
    TokenID: Int,
    Value: Array[Char]
) {
    require(Value.length == 100)
}

case class XmlAttribute (
    Name: Array[Char],
    Value: Array[Char]
) {
    require(Name.length == 50)
    require(Value.length == 100)
}

case class XmlAttributeArray (
    attrs: Array[XmlAttribute],
    nCount: Int
) {
    require(attrs.length == 20)
}

case class Asn1ObjectIdentifier (
    var nCount: Int,
    values: Array[Long] // ULong
) {
    require(values.length == OBJECT_IDENTIFIER_MAX_LENGTH)
    require(nCount >= 0)
}

/* Time Classes
    Asn1LocalTime,					// TIME-OF-DAY    ::= TIME(SETTINGS "Basic=Time Time=HMS Local-or-UTC=L")
    Asn1UtcTime,					//                                    TIME(SETTINGS "Basic=Time Time=HMS Local-or-UTC=Z")
    Asn1LocalTimeWithTimeZone,		//                                    TIME(SETTINGS "Basic=Time Time=HMS Local-or-UTC=LD")
    Asn1Date,						//    DATE ::=                TIME(SETTINGS "Basic=Date Date=YMD Year=Basic")
    Asn1Date_LocalTime,				//    DATE-TIME     ::= TIME(SETTINGS "Basic=Date-Time Date=YMD Year=Basic Time=HMS Local-or-UTC=L")
    Asn1Date_UtcTime,				// 			                TIME(SETTINGS "Basic=Date-Time Date=YMD Year=Basic Time=HMS Local-or-UTC=Z")
    Asn1Date_LocalTimeWithTimeZone	//                                    TIME(SETTINGS "Basic=Date-Time Date=YMD Year=Basic Time=HMS Local-or-UTC=LD")
*/

case class Asn1TimeZone (
    sign: Int, //-1 or +1
    hours: Int,
    mins: Int,
)

case class Asn1TimeWithTimeZone (
    hours: Int,
    mins: Int,
    secs: Int,
    fraction: Int,
    tz: Asn1TimeZone
)

case class Asn1UtcTime (
    hours: Int,
    mins: Int,
    secs: Int,
    fraction: Int,
)

case class Asn1LocalTime (
    hours: Int,
    mins: Int,
    secs: Int,
    fraction: Int,
)

case class Asn1Date (
    years: Int,
    months: Int,
    days: Int,
)

case class Asn1DateLocalTime (
    date: Asn1Date,
    time: Asn1LocalTime
)

case class Asn1DateUtcTime (
    date: Asn1Date,
    time: Asn1UtcTime
)

case class Asn1DateTimeWithTimeZone(
    date: Asn1Date,
    time: Asn1TimeWithTimeZone
)

enum Asn1TimeZoneClass:
    case Asn1TC_LocalTimeStamp, Asn1TC_UtcTimeStamp, Asn1TC_LocalTimeTZStamp

@extern
def Asn1Real_Equal(left: Double, right: Double): Boolean = {
    if left == right then
        true
    else if left == 0.0 then
        right == 0.0 // why??
    else if (left > 0.0 && right < 0.0) || (left < 0.0 && right > 0.0) then
        false
    else if Math.abs(left) > Math.abs(right) then
        Math.abs(right) / Math.abs(left) >= 0.99999
    else
        Math.abs(left) / Math.abs(right) >= 0.99999
}

/**

#######                                      ###
#     # #####       # ######  ####  #####     #  #####  ###### #    # ##### # ###### # ###### #####
#     # #    #      # #      #    #   #       #  #    # #      ##   #   #   # #      # #      #    #
#     # #####       # #####  #        #       #  #    # #####  # #  #   #   # #####  # #####  #    #
#     # #    #      # #      #        #       #  #    # #      #  # #   #   # #      # #      #####
#     # #    # #    # #      #    #   #       #  #    # #      #   ##   #   # #      # #      #   #
####### #####   ####  ######  ####    #      ### #####  ###### #    #   #   # #      # ###### #    #

Object Identifier

**/

def ObjectIdentifier_Init(): Asn1ObjectIdentifier = {
    val pVal: Asn1ObjectIdentifier = Asn1ObjectIdentifier(0, Array.fill(OBJECT_IDENTIFIER_MAX_LENGTH)(0))
    var i: Int = 0
    (while i < OBJECT_IDENTIFIER_MAX_LENGTH do
        decreases(OBJECT_IDENTIFIER_MAX_LENGTH - i)
        pVal.values(i) = 0
        i += 1
    ).invariant(i >= 0 &&& i <= OBJECT_IDENTIFIER_MAX_LENGTH)
    pVal.nCount = 0

    pVal
}

def ObjectIdentifier_isValid(pVal: Asn1ObjectIdentifier): Boolean = {
    return (pVal.nCount >= 2) && (pVal.values(0) <= 2) && (pVal.values(1) <= 39)
}

def RelativeOID_isValid (pVal: Asn1ObjectIdentifier): Boolean = {
    return pVal.nCount > 0
}

def ObjectIdentifier_equal (pVal1: Asn1ObjectIdentifier, pVal2: Asn1ObjectIdentifier): Boolean = {
    require(pVal1.nCount >= 0 && pVal1.nCount <= OBJECT_IDENTIFIER_MAX_LENGTH)
    require(pVal2.nCount >= 0 && pVal2.nCount <= OBJECT_IDENTIFIER_MAX_LENGTH)

    if pVal1.nCount != pVal2.nCount || pVal1.nCount > OBJECT_IDENTIFIER_MAX_LENGTH then
        return false

    var i: Int = 0

    var ret: Boolean = true
    (while i < pVal1.nCount do
        decreases(pVal1.nCount - i)

        ret &= (pVal1.values(i) == pVal2.values(i))
        i += 1
      ).invariant(i >= 0 &&& i <= pVal1.nCount)

    return ret
}
