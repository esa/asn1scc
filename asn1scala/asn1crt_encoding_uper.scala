package asn1crt

import stainless.math.BitVectors._
import stainless.lang.*


def ObjectIdentifier_uper_encode(pBitStrm: BitStream, pVal: Asn1ObjectIdentifier): Unit = {

  val tmp: Array[UInt8] = Array.fill(OBJECT_IDENTIFIER_MAX_LENGTH * (WORD_SIZE + 2))(0)
  val totalSize = Ref[Int](0)

  var i: Int = 0
  ObjectIdentifier_subidentifiers_uper_encode(tmp, totalSize, pVal.values(0) * 40 + pVal.values(1))

  i = 0
  while i < pVal.nCount do
    decreases(pVal.nCount - i)
    ObjectIdentifier_subidentifiers_uper_encode(tmp, totalSize, pVal.values(i))
    i += 1

  if totalSize.x <= 0x7F then
    BitStream_EncodeConstraintWholeNumber(pBitStrm, fromInt(totalSize.x).widen[Int64], 0, 0xFF)
  else
    BitStream_AppendBit(pBitStrm, true)
    BitStream_EncodeConstraintWholeNumber(pBitStrm, fromInt(totalSize.x).widen[Int64], 0, 0x7FFF)

  i = 0
  while i < totalSize.x do
    decreases(totalSize.x - i)
    BitStream_AppendByte0(pBitStrm, tmp(i));
    i += 1
}

def ObjectIdentifier_subidentifiers_uper_encode(encodingBuf: Array[UInt8], pSize: Ref[Int], siValueVal: UInt64): Unit = {
  var lastOctet: flag = false
  val tmp: Array[UInt8] = Array.fill(16)(0)
  var nSize: Int = 0

  var siValue = siValueVal

  while !lastOctet do
    decreases(siValue)
    val curByte: UInt8 = (siValue % 128).narrow[UInt8]
    siValue = siValue / 128
    lastOctet = siValue.toInt == 0
    tmp(nSize) = curByte
    nSize += 1

  var i: Int = 0
  while i < nSize do
    decreases(nSize-i)
    val curByte: UInt8 = if i == nSize-1 then tmp(nSize-i-1) else tmp(nSize-i-1) | 0x80
    encodingBuf(pSize.x) = curByte
    pSize.x += 1
    i += 1

}

