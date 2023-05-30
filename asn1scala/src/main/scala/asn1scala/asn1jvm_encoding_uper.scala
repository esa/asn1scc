package asn1scala

import stainless.math.BitVectors._
import stainless.lang.{None => _, Option => _, Some => _, _}

def ObjectIdentifier_subidentifiers_uper_encode(encodingBuf: Array[UByte], pSize: Ref[Int], siValueVal: ULong): Unit = {
  var lastOctet: Boolean = false
  val tmp: Array[UByte] = Array.fill(16)(0)
  var nSize: Int = 0

  var siValue = siValueVal

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
    encodingBuf(pSize.x) = curByte
    pSize.x += 1
    i += 1

}
def ObjectIdentifier_uper_encode(pBitStrm: BitStream, pVal: Asn1ObjectIdentifier): Unit = {
  val tmp: Array[UByte] = Array.fill(OBJECT_IDENTIFIER_MAX_LENGTH * (WORD_SIZE + 2))(0)
  val totalSize = Ref[Int](0)

  var i: Int = 0
  ObjectIdentifier_subidentifiers_uper_encode(tmp, totalSize, pVal.values(0) * 40 + pVal.values(1))

  i = 0
  while i < pVal.nCount do
    decreases(pVal.nCount - i)
    ObjectIdentifier_subidentifiers_uper_encode(tmp, totalSize, pVal.values(i))
    i += 1

  if totalSize.x <= 0x7F then
    BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize.x.toLong, 0, 0xFF)
  else
    BitStream_AppendBit(pBitStrm, true)
    BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize.x.toLong, 0, 0x7FFF)

  i = 0
  while i < totalSize.x do
    decreases(totalSize.x - i)
    BitStream_AppendByte0(pBitStrm, tmp(i));
    i += 1
}
def RelativeOID_uper_encode (pBitStrm: BitStream, pVal: Asn1ObjectIdentifier): Unit = {
  //a subifentifier (i.e. a component) should not take more than size(asn1SccUint) + 2 to be encoded
  //(the above statement is true for 8 byte integers. If we ever user larger integer then it should be adjusted)
  val tmp: Array[UByte] = Array.fill(OBJECT_IDENTIFIER_MAX_LENGTH * (WORD_SIZE + 2))(0)
  val totalSize = Ref[Int](0)

  var i: Int = 0

  while i < pVal.nCount do
    decreases(pVal.nCount - i)
    ObjectIdentifier_subidentifiers_uper_encode(tmp, totalSize, pVal.values(i))
    i += 1


  if totalSize.x <= 0x7F then
    BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize.x.toLong, 0, 0xFF)
  else
    BitStream_AppendBit(pBitStrm, true)
    BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize.x.toLong, 0, 0x7FFF)

  i = 0
  while i < totalSize.x do
    decreases(totalSize.x - i)
    BitStream_AppendByte0(pBitStrm, tmp(i))
    i += 1
}

def ObjectIdentifier_subidentifiers_uper_decode(pBitStrm: BitStream, pRemainingOctets: Ref[Long], siValue: Ref[ULong]): Boolean = {
  var bLastOctet: Boolean = false
  var curOctetValue: ULong = 0
  siValue.x = 0
  while pRemainingOctets.x > 0 && !bLastOctet do
    decreases(pRemainingOctets.x)
    BitStream_ReadByte(pBitStrm) match
      case None => return false
      case Some(curByte) =>
        pRemainingOctets.x -= 1

        bLastOctet = (curByte & 0x80) == 0
        curOctetValue = (curByte & 0x7F).toLong
        siValue.x = siValue.x << 7
        siValue.x |= curOctetValue

  return true
}
def ObjectIdentifier_uper_decode_lentg(pBitStrm: BitStream, totalSize: Ref[Long]): Boolean = {
  val len2: Ref[Long] = Ref[Long](0)
  if !BitStream_DecodeConstraintWholeNumber(pBitStrm, totalSize, 0, 0xFF) then
    return false
  if totalSize.x > 0x7F then
    if !BitStream_DecodeConstraintWholeNumber(pBitStrm, len2, 0, 0xFF) then
      return false
    totalSize.x <<= 8
    totalSize.x |= len2.x
    totalSize.x &= 0x7FFF

  return true
}
def ObjectIdentifier_uper_decode(pBitStrm: BitStream, pVal: Asn1ObjectIdentifier): Boolean = {
  val si: Ref[ULong] = Ref[ULong](0)
  val totalSize: Ref[Long] = Ref[Long](0)
  ObjectIdentifier_Init(pVal) // TODO: Ref?

  if !ObjectIdentifier_uper_decode_lentg(pBitStrm, totalSize) then
    return false

  if !ObjectIdentifier_subidentifiers_uper_decode(pBitStrm, totalSize, si) then
    return false

  pVal.nCount = 2
  pVal.values(0) = si.x / 40
  pVal.values(1) = si.x % 40
  while totalSize.x > 0 && pVal.nCount < OBJECT_IDENTIFIER_MAX_LENGTH do
    decreases(OBJECT_IDENTIFIER_MAX_LENGTH - pVal.nCount)
    if !ObjectIdentifier_subidentifiers_uper_decode(pBitStrm, totalSize, si) then
      return false

    pVal.values(pVal.nCount) = si.x
    pVal.nCount += 1

  //return true, if totalSize reduced to zero. Otherwise we exit the loop because more components we present than OBJECT_IDENTIFIER_MAX_LENGTH
  return totalSize.x == 0

}
def RelativeOID_uper_decode (pBitStrm: BitStream, pVal: Asn1ObjectIdentifier): Boolean = {
  val si: Ref[ULong] = Ref[ULong](0)
  val totalSize: Ref[Long] = Ref[Long](0)
  ObjectIdentifier_Init(pVal)

  if !ObjectIdentifier_uper_decode_lentg(pBitStrm, totalSize) then
    return false

  while totalSize.x > 0 && pVal.nCount < OBJECT_IDENTIFIER_MAX_LENGTH do
    decreases(OBJECT_IDENTIFIER_MAX_LENGTH - pVal.nCount)
    if !ObjectIdentifier_subidentifiers_uper_decode(pBitStrm, totalSize, si) then
      return false
    pVal.values(pVal.nCount) = si.x
    pVal.nCount += 1

  //return true, if totalSize is zero. Otherwise we exit the loop because more components were present than OBJECT_IDENTIFIER_MAX_LENGTH
  return totalSize.x == 0
}
