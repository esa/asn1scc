package asn1scala

import stainless.lang.{None, Option => _, _}

// type used in ErrorCases
type ErrorCode = Int

// Unsigned datatypes that have no native JVM support
type UByte = Byte
type UShort = Short
type UInt = Int
type ULong = Long
type RealNoRTL = Float
type BooleanNoRTL = Boolean
type ASCIIChar = UByte
type NullType = ASCIIChar

// TODO
type LongNoRTL = Long
type ULongNoRTL = ULong

// Floating Point Types
type asn1Real = Double

val WORD_SIZE = 8
val OBJECT_IDENTIFIER_MAX_LENGTH = 20

val NOT_INITIALIZED_ERR_CODE = 1337
val ERR_INVALID_ENUM_VALUE = 2805

val ber_aux: Array[ULong] = Array(
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
    var ret: ULong = 0
    if v < 0 then
        ret = -v - 1
        ret = ~ret
    else
        ret = v

    ret
}

def uint2int(v: ULong, uintSizeInBytes: Int): Long = {
    var vv = v
    val tmp: ULong = 0x80
    val bIsNegative: Boolean = (vv & (tmp << ((uintSizeInBytes - 1) * 8))) > 0

    if !bIsNegative then
        return v

    var i: Int = WORD_SIZE-1
    while i >= uintSizeInBytes do
        vv |= ber_aux(i)
        i -= 1
    -(~vv) - 1
}

def GetCharIndex(ch: UByte, Set: Array[UByte]): Int =
{
    var i: Int = 0
    while i < Set.length do
        if ch == Set(i) then
            return i
        i += 1
    0
}

def NullType_Initialize(): ASCIIChar = {
    0
}

def asn1Real_Initialize(): asn1Real = {
    0.0
}


case class BitStream (
    var buf: Array[Byte], // UByte
    var currentByte: Int,
    var currentBit: Int,
    // TODO
    var pushDataPrm: Option[Any],
    var fetchDataPrm: Option[Any],
) {
    // TODO: currentByte==buf.length temp possible, but with bitstream_push_data_if_required set to 0 again
    require(currentByte >= 0 && currentByte <= buf.length)
    require(currentBit >= 0 && currentBit < 8)
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

/**

#######                                                                            ###
#         # #####             # ######    ####    #####         #    #####    ###### #        # ##### # ###### # ###### #####
#         # #        #            # #            #        #     #             #    #        # #            ##     #     #     # #            # #            #        #
#         # #####             # #####    #                #             #    #        # #####    # #    #     #     # #####    # #####    #        #
#         # #        #            # #            #                #             #    #        # #            #    # #     #     # #            # #            #####
#         # #        # #        # #            #        #     #             #    #        # #            #     ##     #     # #            # #            #     #
####### #####     ####    ######    ####        #            ### #####    ###### #        #     #     # #            # ###### #        #

Object Identifier

**/



def ObjectIdentifier_Init(): Asn1ObjectIdentifier = {
    var pVal: Asn1ObjectIdentifier = Asn1ObjectIdentifier(0, Array.fill(OBJECT_IDENTIFIER_MAX_LENGTH)(0))
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
    var i: Int = 0
    if pVal1.nCount == pVal2.nCount && pVal1.nCount <= OBJECT_IDENTIFIER_MAX_LENGTH then // TODO: (pVal1 != NULL) && (pVal2 != NULL)
        var ret: Boolean = true
        while i < pVal1.nCount && ret do
            decreases(pVal1.nCount - i)
            ret = (pVal1.values(i) == pVal2.values(i))
            i += 1

        return ret
    else
        return false
}

def CHECK_BIT_STREAM(pBitStrm: BitStream): Unit = {
    assert(pBitStrm.currentByte*8 + pBitStrm.currentBit <= pBitStrm.buf.length*8)
}