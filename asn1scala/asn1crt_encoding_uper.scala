package asn1crt



def ObjectIdentifier_uper_encode(pBitStrm: BitStream, pVal: Asn1ObjectIdentifier): Unit = {
  var tmp = new Array[byte](OBJECT_IDENTIFIER_MAX_LENGTH * (WORD_SIZE + 2))
  var totalSize = Ref[Int](0)

  var i: Integer = 0
  ObjectIdentifier_subidentifiers_uper_encode(tmp, totalSize, pVal.values(0) * 40 + pVal.values(1))

  for i <- 2 to pVal.nCount do
    ObjectIdentifier_subidentifiers_uper_encode(tmp, totalSize, pVal.values(i))

  if totalSize.x <= 0x7F then
    BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize.x, 0, 0xFF)
  else
    BitStream_AppendBit(pBitStrm, 1.asInstanceOf[flag])
    BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize.x, 0, 0x7FFF)

  for i <- 0 to totalSize.x do
    BitStream_AppendByte0(pBitStrm, tmp(i));
}

def ObjectIdentifier_subidentifiers_uper_encode(encodingBuf: Array[byte], pSize: Ref[Int], siValue: asn1SccUint): Unit = {
  var lastOctet: flag = false
  var tmp = new Array[byte](16)
  var nSize: Integer = 0

  var siValueVar = siValue

  while !lastOctet do
    val curByte: byte = (siValueVar % 128.asInstanceOf[asn1SccUint]).asInstanceOf[byte]
    siValueVar = siValueVar / 128.asInstanceOf[asn1SccUint]
    lastOctet = siValueVar == 0.asInstanceOf[asn1SccUint]
    tmp(nSize) = curByte
    nSize += 1

  for i <- 0 to nSize do
    val curByte: byte = if i == nSize-1 then tmp(nSize-i-1) else (tmp(nSize-i-1)|0x80).asInstanceOf[byte]
    encodingBuf(pSize.x) = curByte
    pSize.x += 1
}

