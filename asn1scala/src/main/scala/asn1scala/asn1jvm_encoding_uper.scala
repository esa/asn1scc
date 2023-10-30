package asn1scala

import stainless.math.BitVectors._
import stainless.lang.{None => None, Option => Option, Some => Some, _}

def ObjectIdentifier_subidentifiers_uper_encode(encodingBuf: Array[UByte], pSizeVal: Int, siValueVal: ULong): Int = {
    var lastOctet: Boolean = false
    val tmp: Array[UByte] = Array.fill(16)(0)
    var nSize: Int = 0

    var siValue = siValueVal
    var pSize = pSizeVal

    while !lastOctet do
        decreases(siValue)
        val curByte: UByte = (siValue % 128).toByte
        siValue = siValue / 128
        lastOctet = siValue.toInt == 0
        tmp(nSize) = curByte
        nSize += 1

    var i: Int = 0
    while i < nSize do
        decreases(nSize-i)
        val curByte: UByte = if i == nSize-1 then tmp(nSize-i-1) else (tmp(nSize-i-1) | 0x80).toByte
        encodingBuf(pSize) = curByte
        pSize += 1
        i += 1
    return pSize
}
def ObjectIdentifier_uper_encode(pBitStrm: BitStream, pVal: Asn1ObjectIdentifier): Unit = {
    val tmp: Array[UByte] = Array.fill(OBJECT_IDENTIFIER_MAX_LENGTH * (NO_OF_BYTES_IN_JVM_LONG + 2))(0)
    var totalSize: Int = 0

    var i: Int = 0
    totalSize = ObjectIdentifier_subidentifiers_uper_encode(tmp, totalSize, pVal.values(0) * 40 + pVal.values(1))

    i = 0
    while i < pVal.nCount do
        decreases(pVal.nCount - i)
        totalSize = ObjectIdentifier_subidentifiers_uper_encode(tmp, totalSize, pVal.values(i))
        i += 1

    if totalSize <= 0x7F then
        BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize.toLong, 0, 0xFF)
    else
        pBitStrm.appendBit(true)
        BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize.toLong, 0, 0x7FFF)

    i = 0
    while i < totalSize do
        decreases(totalSize - i)
        pBitStrm.appendByte0(tmp(i))
        i += 1
}
def RelativeOID_uper_encode (pBitStrm: BitStream, pVal: Asn1ObjectIdentifier): Unit = {
    //a subifentifier (i.e. a component) should not take more than size(asn1SccUint) + 2 to be encoded
    //(the above statement is true for 8 byte integers. If we ever user larger integer then it should be adjusted)
    val tmp: Array[UByte] = Array.fill(OBJECT_IDENTIFIER_MAX_LENGTH * (NO_OF_BYTES_IN_JVM_LONG + 2))(0)
    var totalSize: Int = 0

    var i: Int = 0

    while i < pVal.nCount do
        decreases(pVal.nCount - i)
        totalSize = ObjectIdentifier_subidentifiers_uper_encode(tmp, totalSize, pVal.values(i))
        i += 1


    if totalSize <= 0x7F then
        BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize.toLong, 0, 0xFF)
    else
        pBitStrm.appendBit(true)
        BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize.toLong, 0, 0x7FFF)

    i = 0
    while i < totalSize do
        decreases(totalSize - i)
        pBitStrm.appendByte0(tmp(i))
        i += 1
}

def ObjectIdentifier_subidentifiers_uper_decode(pBitStrm: BitStream, pRemainingOctetsVal: Long): Option[(Long, ULong)] = {
    var bLastOctet: Boolean = false
    var curOctetValue: ULong = 0
    var siValue: ULong = 0
    var pRemainingOctets: Long = pRemainingOctetsVal
    while pRemainingOctets > 0 && !bLastOctet do
        decreases(pRemainingOctets)
        pBitStrm.readByte() match
            case None() => return None()
            case Some(curByte) =>
                pRemainingOctets -= 1

                bLastOctet = (curByte & 0x80) == 0
                curOctetValue = (curByte & 0x7F).toLong
                siValue = siValue << 7
                siValue |= curOctetValue

    return Some((pRemainingOctets, siValue))
}


def ObjectIdentifier_uper_decode_lentg(pBitStrm: BitStream): Option[Long] = {
    var totalSize: Long = 0

    BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 0xFF) match
        case None() => return None()
        case Some(l) => totalSize = l

    if totalSize > 0x7F then
        BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 0xFF) match
            case None() => return None()
            case Some(l) =>
                totalSize <<= 8
                totalSize |= l
                totalSize &= 0x7FFF

    return Some(totalSize)
}
def ObjectIdentifier_uper_decode(pBitStrm: BitStream): OptionMut[Asn1ObjectIdentifier] = {
    var si: ULong = 0
    var totalSize: Long = 0
    val pVal = ObjectIdentifier_Init()

    ObjectIdentifier_uper_decode_lentg(pBitStrm) match
        case None() => return NoneMut()
        case Some(l) => totalSize = l

    ObjectIdentifier_subidentifiers_uper_decode(pBitStrm, totalSize) match
        case None() => return NoneMut()
        case Some((l, ul)) =>
            totalSize = l
            si = ul

    pVal.nCount = 2
    pVal.values(0) = si / 40
    pVal.values(1) = si % 40
    while totalSize > 0 && pVal.nCount < OBJECT_IDENTIFIER_MAX_LENGTH do
        decreases(OBJECT_IDENTIFIER_MAX_LENGTH - pVal.nCount)

        ObjectIdentifier_subidentifiers_uper_decode(pBitStrm, totalSize) match
            case None() => return NoneMut()
            case Some((l, ul)) =>
                totalSize = l
                si = ul

        pVal.values(pVal.nCount) = si
        pVal.nCount += 1

    //return true, if totalSize reduced to zero. Otherwise we exit the loop because more components we present than OBJECT_IDENTIFIER_MAX_LENGTH
    if totalSize == 0 then
        SomeMut(pVal)
    else
        NoneMut()

}
def RelativeOID_uper_decode (pBitStrm: BitStream): OptionMut[Asn1ObjectIdentifier] = {
    var si: ULong = 0
    var totalSize: Long = 0
    val pVal: Asn1ObjectIdentifier = ObjectIdentifier_Init()

    ObjectIdentifier_uper_decode_lentg(pBitStrm) match
        case None() => return NoneMut()
        case Some(l) => totalSize = l

    while totalSize > 0 && pVal.nCount < OBJECT_IDENTIFIER_MAX_LENGTH do
        decreases(OBJECT_IDENTIFIER_MAX_LENGTH - pVal.nCount)
        ObjectIdentifier_subidentifiers_uper_decode(pBitStrm, totalSize) match
            case None() => return NoneMut()
            case Some((l, ul)) =>
                totalSize = l
                si = ul
        pVal.values(pVal.nCount) = si
        pVal.nCount += 1

    //return true, if totalSize is zero. Otherwise we exit the loop because more components were present than OBJECT_IDENTIFIER_MAX_LENGTH
    if totalSize == 0 then
        SomeMut(pVal)
    else
        NoneMut()
}
