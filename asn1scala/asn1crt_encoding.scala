package asn1crt

import stainless.math.BitVectors.*
import stainless.lang.*

val masks: Array[UInt8] = Array(0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 )
val masksb: Array[UInt8] = Array(0x0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF)
val masks2: Array[UInt32] = Array(
  0x0,
  0xFF,
  0xFF00,
  0xFF0000,
  fromLong (0xFF000000L).toUnsigned[UInt64].narrow[UInt32]
)


def BitStream_EncodeNonNegativeInteger32Neg(pBitStrm: BitStream, v: UInt32, negate: flag): Unit = {
  var cc: UInt32 = 0
  var curMask: UInt32 = 0
  var pbits: UInt32 = 0

  if v == fromInt(0) then
    return ()

  if v < 0x100 then
    cc = 8
    curMask = 0x80
  else if v < 0x10000 then
    cc = 16
    curMask = 0x8000
  else if v < 0x1000000 then
    cc = 24
    curMask = 0x800000
  else
    cc = 32
    curMask = fromLong(0x80000000L).toUnsigned[UInt64].narrow[UInt32]

  while (v & curMask) == fromInt(0) do
    decreases(cc)
    curMask >>= 1
    cc -= 1

  pbits = cc % 8
  if pbits > 0 then
    cc -= pbits
    BitStream_AppendPartialByte(pBitStrm, (v >> cc).narrow[UInt8], pbits.narrow[UInt8], negate)

  while cc > 0 do
    decreases(cc)
    val t1: UInt32 = v & masks2(cc >> 3)
    cc -= 8
    BitStream_AppendByte(pBitStrm, (t1 >> cc).narrow[UInt8], negate)
}

def BitStream_EncodeNonNegativeInteger(pBitStrm: BitStream, v: UInt64): Unit = {
  if WORD_SIZE == 8 then
    if v < fromLong(0x100000000L).toUnsigned[UInt64] then
      BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, v.narrow[UInt32], false)
    else
      val hi: UInt32 = (v >> 32).narrow[UInt32]
      val lo: UInt32 = v.narrow[UInt32]
      BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, hi, false)

      val nBits: Int = GetNumberOfBitsForNonNegativeInteger(lo.widen[UInt64])
      BitStream_AppendNBitZero(pBitStrm, 32 - nBits)
      BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, lo, false)
  else
    BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, v.narrow[UInt32], false)
}

def BitStream_EncodeConstraintWholeNumber(pBitStrm: BitStream, v: Int64, min: Int64, max: Int64): Unit = {
  require(min <= max)
  val range = (max - min).toUnsigned[UInt64]
  if range == fromInt(0) then
    return

  val nRangeBits: Int = GetNumberOfBitsForNonNegativeInteger(range)
  val nBits: Int = GetNumberOfBitsForNonNegativeInteger((v - min).toUnsigned[UInt64])
  BitStream_AppendNBitZero(pBitStrm, nRangeBits - nBits);
  BitStream_EncodeNonNegativeInteger(pBitStrm, (v - min).toUnsigned[UInt64])
}

def BitStream_AppendBit(pBitStrm: BitStream, v: flag): Unit = {
  if v then
    pBitStrm.buf(pBitStrm.currentByte) |= masks(pBitStrm.currentBit)
  else
    val nmask = ~masks(pBitStrm.currentBit)
    pBitStrm.buf(pBitStrm.currentByte) &= nmask

  if pBitStrm.currentBit < 7 then
    pBitStrm.currentBit += 1
  else
    pBitStrm.currentBit = 0
    pBitStrm.currentByte += 1
    //bitstream_push_data_if_required(pBitStrm);

  assert(pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.count * 8);
}

def BitStream_AppendNBitZero(pBitStrm: BitStream, nbits: Int): Unit = {
  val totalBits: Int = pBitStrm.currentBit + nbits
  val totalBytes: Int = totalBits / 8
  pBitStrm.currentBit = totalBits % 8
  //pBitStrm->currentByte += totalBits / 8;

  if pBitStrm.currentByte + totalBytes <= pBitStrm.count then
    pBitStrm.currentByte += totalBytes
    //bitstream_push_data_if_required(pBitStrm)
  else
    val extraBytes: Int = (pBitStrm.currentByte + totalBytes - pBitStrm.count)
    pBitStrm.currentByte = pBitStrm.count
    //bitstream_push_data_if_required(pBitStrm)
    pBitStrm.currentByte = extraBytes
}


def BitStream_AppendByte(pBitStrm: BitStream, vv: UInt8, negate: flag): Unit = {
  //static UInt8 masksb[] = { 0x0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF };
  val cb: UInt8 = fromInt(pBitStrm.currentBit).toUnsigned[UInt32].narrow[UInt8]
  val ncb: UInt8 = 8 - cb

  var v = vv
  if negate then
    v = ~v

  var mask: UInt8 = ~masksb(ncb)

  pBitStrm.buf(pBitStrm.currentByte) &= mask
  pBitStrm.currentByte += 1
  pBitStrm.buf(pBitStrm.currentByte) |= (v >> cb)
  //bitstream_push_data_if_required(pBitStrm)

  assert(pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.count * 8)

  if cb > 0 then
    mask = ~mask
    pBitStrm.buf(pBitStrm.currentByte) &= mask
    pBitStrm.buf(pBitStrm.currentByte) |= (v << ncb)
}

def BitStream_AppendByte0(pBitStrm: BitStream, v: UInt8): flag = {
  val cb: UInt8 = fromInt(pBitStrm.currentBit).toUnsigned[UInt32].narrow[UInt8]
  val ncb: UInt8 = 8-cb

  var mask = ~masks(ncb)

  pBitStrm.buf(pBitStrm.currentByte) &= mask
  pBitStrm.currentByte += 1
  pBitStrm.buf(pBitStrm.currentByte) |= (v >> cb)
  //bitstream_push_data_if_required(pBitStrm);

  if cb > 0 then
    if pBitStrm.currentByte >= pBitStrm.count then
      return false
    mask = ~mask
    pBitStrm.buf(pBitStrm.currentByte) &= mask
    pBitStrm.buf(pBitStrm.currentByte) |= (v << cb)

  true
}

def BitStream_AppendPartialByte(pBitStrm: BitStream, vv: UInt8, nbits: UInt8, negate: flag): Unit = {
  val cb: UInt8 = fromInt(pBitStrm.currentBit).toUnsigned[UInt32].narrow[UInt8]
  val totalBits: UInt8 = cb + nbits
  val ncb: UInt8 = 8 - cb

  var v = vv
  if negate then
    v = masksb(nbits) & (~v)

  val mask1: UInt8 = ~masksb(ncb)

  if (totalBits <= 8) {
    //static UInt8 masksb[] = { 0x0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF };
    val mask2: UInt8 = masksb(8 - totalBits)
    val mask: UInt8 = mask1 | mask2
    //e.g. current bit = 3 --> mask =  1110 0000
    //nbits = 3 --> totalBits = 6
    //                         mask=   1110 0000
    //                         and     0000 0011 <- masks[totalBits - 1]
    //	                              -----------
    //					final mask     1110 0011
    pBitStrm.buf(pBitStrm.currentByte) &= mask
    pBitStrm.buf(pBitStrm.currentByte) |= (v << (8 - totalBits))
    pBitStrm.currentBit += nbits.widen[UInt32].toSigned[Int32].asInstanceOf[Int]
    if (pBitStrm.currentBit == 8) {
      pBitStrm.currentBit = 0
      pBitStrm.currentByte += 1
      //bitstream_push_data_if_required(pBitStrm);
    }
  } else {
    val totalBitsForNextByte: UInt8 = totalBits - 8
    pBitStrm.buf(pBitStrm.currentByte) &= mask1
    pBitStrm.currentByte += 1
    pBitStrm.buf(pBitStrm.currentByte) |= (v >> totalBitsForNextByte)
    //bitstream_push_data_if_required(pBitStrm);
    val mask: UInt8 = ~masksb(8 - totalBitsForNextByte)
    pBitStrm.buf(pBitStrm.currentByte) &= mask
    pBitStrm.buf(pBitStrm.currentByte) |= (v << (8 - totalBitsForNextByte))
    pBitStrm.currentBit = totalBitsForNextByte.widen[UInt32].toSigned[Int32].asInstanceOf[Int]
  }

  assert(pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.count * 8)
}

def GetNumberOfBitsForNonNegativeInteger(v: UInt64): Int = {
  if WORD_SIZE == 8 then
    if v < fromLong(0x100000000L).toUnsigned[UInt64] then
      return GetNumberOfBitsForNonNegativeInteger32(v.narrow[UInt32])
    else
      val hi: UInt32 = (v >> 32).narrow[UInt32]
      return 32 + GetNumberOfBitsForNonNegativeInteger32(hi)
  else
    return GetNumberOfBitsForNonNegativeInteger32(v.narrow[UInt32])
}

def GetNumberOfBitsForNonNegativeInteger32(vv: UInt32): Int = {
  var ret: Int = 0

  var v = vv
  if v < 0x100 then
    ret = 0
  else if v < 0x10000 then
    ret = 8
    v = v >> 8
  else if v < 0x1000000 then
    ret = 16
    v = v >> 16
  else
    ret = 24
    v = v >> 24

  while v > 0 do
    decreases(v)
    v = v >> 1
    ret += 1

  return ret
}