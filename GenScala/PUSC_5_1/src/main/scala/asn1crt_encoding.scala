package asn1crt

import stainless.math.BitVectors._
import stainless.lang._

val masks: Array[UInt8] = Array(0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 )
val masksb: Array[UInt8] = Array(0x0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF)
val masks2: Array[UInt32] = Array(
  0x0,
  0xFF,
  0xFF00,
  0xFF0000,
  fromLong(0xFF000000L).toUnsigned[UInt64].narrow[UInt32]
)


/***********************************************************************************************/
/**  Byte Stream Functions                                                                    **/
/***********************************************************************************************/
def ByteStream_Init(pStrm: ByteStream, count: Int): Unit = {
  pStrm.buf = Array.fill(count)(0)
  pStrm.currentByte = 0
  pStrm.EncodeWhiteSpace = false
}

def ByteStream_AttachBuffer(pStrm: ByteStream, buf: Array[UInt8]): Unit = {
  // pStrm.buf = buf // TODO: fix illegal aliasing
  pStrm.currentByte = 0
}

def ByteStream_GetLength(pStrm: ByteStream): Int32 = {
  fromInt(pStrm.currentByte)
}

/***********************************************************************************************/
/**  Bit Stream Functions                                                                     **/
/***********************************************************************************************/
def BitString_equal(arr1: Array[UInt8], arr2: Array[UInt8]): Boolean = {
  // TODO
  true
  //return
  //  (nBitsLength1 == nBitsLength2) &&
  //    (nBitsLength1 / 8 == 0 || memcmp(arr1, arr2, nBitsLength1 / 8) == 0) &&
  //    (nBitsLength1 % 8 > 0 ? (arr1[nBitsLength1 / 8] >> (8 - nBitsLength1 % 8) == arr2[nBitsLength1 / 8] >> (8 - nBitsLength1 % 8)): TRUE);
}

/*
def BitStream_Init(pBitStrm: BitStream, count: Int): Unit = {
  pBitStrm.buf = Array.fill(count)(0)
  pBitStrm.currentByte = 0
  pBitStrm.currentBit = 0
  // TODO
  //pBitStrm.pushDataPrm = null
  //pBitStrm.fetchDataPrm = null
}

//def BitStream_Init2(pBitStrm: BitStream, count: Int, pushDataPrm, fetchDataPrm): Unit = {
def BitStream_Init2(pBitStrm: BitStream, count: Int): Unit = {
  BitStream_Init(pBitStrm, count)
  // TODO
  //pBitStrm.fetchDataPrm = fetchDataPrm
  //pBitStrm.pushDataPrm = pushDataPrm
}
*/

def BitStream_AttachBuffer(pBitStrm: BitStream, buf: Array[UInt8]): Unit = {
  //pBitStrm.buf = buf // TODO: fix ilegal aliasing
  pBitStrm.currentByte = 0
  pBitStrm.currentBit = 0
  // TODO
  //pBitStrm.pushDataPrm = NULL
  //pBitStrm.fetchDataPrm = NULL
}

//def BitStream_AttachBuffer2(pBitStrm: BitStream, buf: Array[UInt8], pushDataPrm, fetchDataPrm): Unit = {
def BitStream_AttachBuffer2(pBitStrm: BitStream, buf: Array[UInt8]): Unit = {
  BitStream_AttachBuffer(pBitStrm, buf)
  // TODO
  //pBitStrm.pushDataPrm = pushDataPrm
  //pBitStrm.fetchDataPrm = fetchDataPrm
}

def BitStream_GetLength(pBitStrm: BitStream): Int32 = {
  var ret: Int32 = fromInt(pBitStrm.currentByte)
  if pBitStrm.currentBit > 0 then
    ret += 1
  ret
}

/**
Append bit one.

Example
      cur bit = 3
   x x x |
  |_|_|_|_|_|_|_|_|
   0 1 2 3 4 5 6 7

    xxxy????
or  00010000
------------
    xxx1????
**/

def BitStream_AppendBitOne(pBitStrm: BitStream): Unit = {
  pBitStrm.buf(pBitStrm.currentByte) |= masks(pBitStrm.currentBit)

  if pBitStrm.currentBit < 7 then
    pBitStrm.currentBit += 1
  else
    pBitStrm.currentBit = 0
    pBitStrm.currentByte += 1
    bitstream_push_data_if_required(pBitStrm)
  assert(pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.buf.length * 8)
}

/**
  Append bit zero.

  Example
  cur bit = 3
  x x x |
  |_|_|_|_|_|_|_|_|
  0 1 2 3 4 5 6 7

      xxxy????
  and 11101111
      ------------
      xxx0????
**/
def BitStream_AppendBitZero(pBitStrm: BitStream): Unit = {
  val nmask = ~masks(pBitStrm.currentBit)
  pBitStrm.buf(pBitStrm.currentByte) &= nmask
  if pBitStrm.currentBit < 7 then
    pBitStrm.currentBit += 1
  else
    pBitStrm.currentBit = 0
    pBitStrm.currentByte += 1
    bitstream_push_data_if_required(pBitStrm)
  assert(pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.buf.length * 8)
}

def BitStream_AppendNBitZero(pBitStrm: BitStream, nbits: Int): Unit = {
  require(nbits < Int.MaxValue - pBitStrm.currentBit)
  val totalBits: Int = pBitStrm.currentBit + nbits
  val totalBytes: Int = totalBits / 8

  val newCurrentBit = totalBits % 8
  assert(0 <= newCurrentBit && newCurrentBit < 8)
  pBitStrm.currentBit = newCurrentBit
  //pBitStrm->currentByte += totalBits / 8;

  if pBitStrm.currentByte <= pBitStrm.buf.length - totalBytes then
    pBitStrm.currentByte += totalBytes
    bitstream_push_data_if_required(pBitStrm)
  else
    val extraBytes: Int = pBitStrm.currentByte + totalBytes - pBitStrm.buf.length
    pBitStrm.currentByte = pBitStrm.buf.length
    bitstream_push_data_if_required(pBitStrm)
    pBitStrm.currentByte = extraBytes
}

def BitStream_AppendNBitOne(pBitStrm: BitStream, nbitsVal: Int): Unit = {
  var nbits = nbitsVal
  while nbits >= 8 do
    decreases(nbits)
    BitStream_AppendByte(pBitStrm, 0xFF, false)
    nbits -= 8

  var i = 0
  while i < nbits do
    decreases(nbits - i)
    BitStream_AppendBitOne(pBitStrm)
    i+= 1
}

def BitStream_AppendBits(pBitStrm: BitStream, srcBuffer: Array[UInt8], nbits: Int): Unit = {
  var lastByte: UInt8 = 0

  val bytesToEncode: Int = nbits / 8
  val remainingBits: UInt8 = fromInt(nbits % 8).toUnsigned[UInt32].narrow[UInt8]

  BitStream_EncodeOctetString_no_length(pBitStrm, srcBuffer, bytesToEncode)

  if remainingBits > 0 then
    lastByte = srcBuffer(bytesToEncode) >> (8 - remainingBits)
    BitStream_AppendPartialByte(pBitStrm, lastByte, remainingBits, false)
}

def BitStream_AppendBit(pBitStrm: BitStream, v: Boolean): Unit = {
  // TODO: currentByte=pBitStrm.buf.length temp possible, but with bitstream_push_data_if_required set to 0 again
  require(pBitStrm.currentByte + (pBitStrm.currentBit+1) / 8 < pBitStrm.buf.length)
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
    bitstream_push_data_if_required(pBitStrm)

  assert(pBitStrm.currentByte + pBitStrm.currentBit/8 <= pBitStrm.buf.length)
}

def BitStream_ReadBit(pBitStrm: BitStream, v: Ref[Boolean]): Boolean = {
  v.x = (pBitStrm.buf(pBitStrm.currentByte) & masks(pBitStrm.currentBit)) != fromInt(0)

  if pBitStrm.currentBit < 7 then
    pBitStrm.currentBit += 1
  else
    pBitStrm.currentBit = 0
    pBitStrm.currentByte += 1
    bitstream_fetch_data_if_required(pBitStrm)

  return pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.buf.length * 8
}

def BitStream_PeekBit(pBitStrm: BitStream): Boolean = {
  (pBitStrm.buf(pBitStrm.currentByte) & masks(pBitStrm.currentBit)) > 0
}

/**
Append byte.

Example
cur bit = 3
       |
 x x x b b b b b b b b
|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
 0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7

first byte
    xxx?????
and 11100000 (mask)
------------
    xxx00000
or  000bbbbb
------------
    xxxbbbbb

**/
def BitStream_AppendByte(pBitStrm: BitStream, vVal: UInt8, negate: Boolean): Unit = {
  //static UInt8 masksb[] = { 0x0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF };
  val cb: UInt8 = fromInt(pBitStrm.currentBit).toUnsigned[UInt32].narrow[UInt8]
  val ncb: UInt8 = 8 - cb

  var v = vVal
  if negate then
    v = ~v

  var mask: UInt8 = ~masksb(ncb)

  pBitStrm.buf(pBitStrm.currentByte) &= mask
  pBitStrm.currentByte += 1
  pBitStrm.buf(pBitStrm.currentByte) |= (v >> cb)
  //bitstream_push_data_if_required(pBitStrm)

  assert(pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.buf.length * 8)

  if cb > 0 then
    mask = ~mask
    pBitStrm.buf(pBitStrm.currentByte) &= mask
    pBitStrm.buf(pBitStrm.currentByte) |= (v << ncb)
}

def BitStream_AppendByte0(pBitStrm: BitStream, v: UInt8): Boolean = {
  val cb: UInt8 = fromInt(pBitStrm.currentBit).toUnsigned[UInt32].narrow[UInt8]
  val ncb: UInt8 = 8-cb

  var mask = ~masks(ncb)

  pBitStrm.buf(pBitStrm.currentByte) &= mask
  pBitStrm.currentByte += 1
  pBitStrm.buf(pBitStrm.currentByte) |= (v >> cb)
  //bitstream_push_data_if_required(pBitStrm);

  if cb > 0 then
    if pBitStrm.currentByte >= pBitStrm.buf.length then
      return false
    mask = ~mask
    pBitStrm.buf(pBitStrm.currentByte) &= mask
    pBitStrm.buf(pBitStrm.currentByte) |= (v << cb)

  true
}

def BitStream_AppendByteArray(pBitStrm: BitStream, arr: Array[UInt8]): Boolean = {
  //static byte  masks[] = { 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 };
  //static byte masksb[] = { 0x00, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F, 0xFF };

  val arr_len = arr.length

  val cb: UInt8 = fromInt(pBitStrm.currentBit).toUnsigned[UInt32].narrow[UInt8]
  val ncb: UInt8 = 8 - cb

  val mask: UInt8 = ~masksb(ncb)
  val nmask: UInt8 = ~mask

  //if (pBitStrm->currentByte + (int)arr_len + (cb > 0 ? 1 : 0) >= pBitStrm->count)
  if (pBitStrm.currentByte+arr_len)*8+pBitStrm.currentBit > pBitStrm.buf.length*8 then
    return false

  if arr_len > 0 then
    val v: UInt8 = arr(0)
    pBitStrm.buf(pBitStrm.currentByte) &= mask      //make zero right bits (i.e. the ones that will get the new value)
    pBitStrm.buf(pBitStrm.currentByte)|= (v >> cb)  //shift right and then populate current byte
    pBitStrm.currentByte += 1
    bitstream_push_data_if_required(pBitStrm)

    pBitStrm.buf(pBitStrm.currentByte) &= nmask
    pBitStrm.buf(pBitStrm.currentByte) |= (v << ncb)

  var i: Int = 1
  while i < arr_len-1 do
    decreases(arr_len-1-i)
    val v: UInt8 = arr(i)
    val v1: UInt8 = (v >> cb)
    val v2: UInt8 = (v << ncb)
    pBitStrm.buf(pBitStrm.currentByte) |= v1 //shift right and then populate current byte
    pBitStrm.currentByte += 1
    bitstream_push_data_if_required(pBitStrm)
    pBitStrm.buf(pBitStrm.currentByte) |= v2
    i += 1

  if arr_len - 1 > 0 then
    val v: UInt8 = arr(arr_len - 1)
    pBitStrm.buf(pBitStrm.currentByte) &= mask       //make zero right bits (i.e. the ones that will get the new value)
    pBitStrm.buf(pBitStrm.currentByte) |= (v >> cb)  //shift right and then populate current byte
    pBitStrm.currentByte += 1
    bitstream_push_data_if_required(pBitStrm)

    if cb > 0 then
      pBitStrm.buf(pBitStrm.currentByte) &= nmask
      pBitStrm.buf(pBitStrm.currentByte) |= (v << ncb)

  true
}

def BitStream_ReadByte(pBitStrm: BitStream, v: Ref[UInt8]): Boolean = {
  val cb: UInt8 = fromInt(pBitStrm.currentBit).toUnsigned[UInt32].narrow[UInt8]
  val ncb: UInt8 = 8 - cb

  v.x = pBitStrm.buf(pBitStrm.currentByte) << cb
  pBitStrm.currentByte += 1
  bitstream_fetch_data_if_required(pBitStrm)

  if cb > 0 then
    v.x = v.x | pBitStrm.buf(pBitStrm.currentByte) >> ncb

  return pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.buf.length * 8
}

def BitStream_ReadByteArray(pBitStrm: BitStream, arr: Array[UInt8]): Boolean = {
  val cb: UInt8 = fromInt(pBitStrm.currentBit).toUnsigned[UInt32].narrow[UInt8]
  val ncb: UInt8 = 8 - cb

  if (pBitStrm.currentByte+arr.length)*8+cb.toInt > pBitStrm.buf.length*8 then
    return false

  var i: Int = 0
  while i < arr.length do
    decreases(arr.length - i)
    arr(i) = pBitStrm.buf(pBitStrm.currentByte) << cb
    pBitStrm.currentByte += 1
    bitstream_fetch_data_if_required(pBitStrm)
    arr(i) |= pBitStrm.buf(pBitStrm.currentByte) >> ncb
    i += 1

  true
}

def BitStream_ReadBits(pBitStrm: BitStream, BuffToWrite: Array[UInt8], nbits: Int): Boolean = {
  val bytesToRead: Int = nbits / 8
  val remainingBits: UInt8 = fromInt(nbits % 8).toUnsigned[UInt32].narrow[UInt8]
  var ret: Boolean = false

  ret = BitStream_DecodeOctetString_no_length(pBitStrm, BuffToWrite, bytesToRead)

  if ret && remainingBits > 0 then
    val v: Ref[UInt8] = Ref[UInt8](0)
    val ret2 = BitStream_ReadPartialByte(pBitStrm, v, remainingBits)
    BuffToWrite(bytesToRead) = v.x
    if !ret2 then
      return false
    BuffToWrite(bytesToRead) = BuffToWrite(bytesToRead) << (8 - remainingBits)

  ret
}


/* nbits 1..7*/
def BitStream_AppendPartialByte(pBitStrm: BitStream, vVal: UInt8, nbits: UInt8, negate: Boolean): Unit = {
  val cb: UInt8 = fromInt(pBitStrm.currentBit).toUnsigned[UInt32].narrow[UInt8]
  val totalBits: UInt8 = cb + nbits
  val ncb: UInt8 = 8 - cb

  var v = vVal
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
    pBitStrm.currentBit += nbits.toInt
    if pBitStrm.currentBit == 8 then
      pBitStrm.currentBit = 0
      pBitStrm.currentByte += 1
    //bitstream_push_data_if_required(pBitStrm);

  } else {
    val totalBitsForNextByte: UInt8 = totalBits - 8
    pBitStrm.buf(pBitStrm.currentByte) &= mask1
    pBitStrm.currentByte += 1
    pBitStrm.buf(pBitStrm.currentByte) |= (v >> totalBitsForNextByte)
    //bitstream_push_data_if_required(pBitStrm);
    val mask: UInt8 = ~masksb(8 - totalBitsForNextByte)
    pBitStrm.buf(pBitStrm.currentByte) &= mask
    pBitStrm.buf(pBitStrm.currentByte) |= (v << (8 - totalBitsForNextByte))
    pBitStrm.currentBit = totalBitsForNextByte.toInt
  }

  assert(pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.buf.length * 8)
}

/* nbits 1..7*/
def BitStream_ReadPartialByte(pBitStrm: BitStream, v: Ref[UInt8], nbits: UInt8): Boolean = {
  val cb: UInt8 = fromInt(pBitStrm.currentBit).toUnsigned[UInt32].narrow[UInt8]
  val totalBits: UInt8 = cb + nbits

  if (totalBits <= 8) {
    v.x = (pBitStrm.buf(pBitStrm.currentByte) >> (8 - totalBits)) & masksb(nbits)
    pBitStrm.currentBit += nbits.toInt
    if pBitStrm.currentBit == 8 then
      pBitStrm.currentBit = 0
      pBitStrm.currentByte += 1
    bitstream_fetch_data_if_required(pBitStrm)

  } else {
    var totalBitsForNextByte: UInt8 = totalBits - 8
    v.x = pBitStrm.buf(pBitStrm.currentByte) << totalBitsForNextByte
    pBitStrm.currentByte += 1
    bitstream_fetch_data_if_required(pBitStrm)
    v.x |= pBitStrm.buf(pBitStrm.currentByte) >> (8 - totalBitsForNextByte)
    v.x &= masksb(nbits)
    pBitStrm.currentBit = totalBitsForNextByte.toInt
  }

  return pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.buf.length * 8
}


/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/
/**   Integer Functions                                                                       **/
/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/
def BitStream_EncodeNonNegativeInteger32Neg(pBitStrm: BitStream, v: UInt32, negate: Boolean): Unit = {
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
def BitStream_DecodeNonNegativeInteger32Neg(pBitStrm: BitStream, v: Ref[UInt32], nBitsVal: Int): Boolean = {
  val b: Ref[UInt8] = Ref[UInt8](0)
  v.x = 0

  var nBits = nBitsVal
  while nBits >= 8 do
    decreases(nBits)
    v.x = v.x << 8
    if !BitStream_ReadByte(pBitStrm, b) then
      return false
    v.x = v.x | b.x.widen[UInt32]
    nBits -= 8

  if nBits != 0 then
    v.x = v.x << fromInt(nBits).toUnsigned[UInt32]
    if !BitStream_ReadPartialByte(pBitStrm, b, fromInt(nBits).toUnsigned[UInt32].narrow[UInt8]) then
      return false
    v.x = v.x | b.x.widen[UInt32]

  return true
}
def BitStream_EncodeNonNegativeInteger(pBitStrm: BitStream, v: UInt64): Unit = {
  // TODO: support WORD_SIZE=4?
  //if WORD_SIZE == 8 then
  if v < fromLong(0x100000000L).toUnsigned[UInt64] then
    BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, v.narrow[UInt32], false)
  else
    val hi: UInt32 = (v >> 32).narrow[UInt32]
    val lo: UInt32 = v.narrow[UInt32]
    BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, hi, false)

    val nBits: Int = GetNumberOfBitsForNonNegativeInteger(lo.widen[UInt64])
    BitStream_AppendNBitZero(pBitStrm, 32 - nBits)
    BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, lo, false)
  //else
  //  BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, v.narrow[UInt32], false)
}
def BitStream_DecodeNonNegativeInteger(pBitStrm: BitStream, v: Ref[UInt64], nBits: Int): Boolean = {
  // TODO: support WORD_SIZE=4?
  // if WORD_SIZE == 8 then
  val hi: Ref[UInt32] = Ref[UInt32](0)
  val lo: Ref[UInt32] = Ref[UInt32](0)
  var ret: Boolean = false

  if nBits <= 32 then
    ret = BitStream_DecodeNonNegativeInteger32Neg(pBitStrm, lo, nBits)
    v.x = lo.x.widen[UInt64]
    return ret

  val hi_ret = BitStream_DecodeNonNegativeInteger32Neg(pBitStrm, hi, 32)
  val lo_ret = BitStream_DecodeNonNegativeInteger32Neg(pBitStrm, lo, nBits - 32)
  ret = hi_ret && lo_ret

  v.x = hi.x.widen[UInt64]
  v.x = v.x << fromInt(nBits - 32).widen[Int64].toUnsigned[UInt64]
  v.x |= lo.x.widen[UInt64]
  return ret
  //else
  //  return BitStream_DecodeNonNegativeInteger32Neg(pBitStrm, v, nBits)
}

def BitStream_EncodeNonNegativeIntegerNeg(pBitStrm: BitStream, v: UInt64, negate: Boolean): Unit = {
  //if WORD_SIZE == 8 then
  if v < fromLong(0x100000000L).toUnsigned[UInt64] then
    BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, v.narrow[UInt32], negate)
  else
    val hi: UInt32 = (v >> 32).narrow[UInt32]
    var lo: UInt32 = v.narrow[UInt32]
    BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, hi, negate)

    /*bug !!!!*/
    if negate then
      lo = ~lo
    val nBits = GetNumberOfBitsForNonNegativeInteger(lo.widen[UInt64])
    BitStream_AppendNBitZero(pBitStrm, 32 - nBits)
    BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, lo, false)
  //else
  //  BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, v, negate)

}

def GetNumberOfBitsForNonNegativeInteger32(vVal: UInt32): Int = {
  var ret: Int = 0

  var v = vVal
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

def GetLengthInBytesOfUInt (v: UInt64): Int = {
  var ret: Int = 0
  var v32: UInt32 = v.narrow[UInt32]
  //if (WORD_SIZE == 8) {
  if v > fromInt(0xFFFFFFFF).toUnsigned[UInt32].widen[UInt64] then
    ret = 4
    v32 = (v >> 32).narrow[UInt32]
  // }

  if v32 < fromInt(0x100).toUnsigned[UInt32] then
    return ret + 1
  if v32 < fromInt(0x10000).toUnsigned[UInt32] then
    return ret + 2
  if v32 < fromInt(0x1000000).toUnsigned[UInt32] then
    return ret + 3

  return ret + 4
}

def GetLengthSIntHelper(v: UInt64): Int = {
  var ret: Int = 0
  var v32: UInt32 = v.narrow[UInt32]
  //#if WORD_SIZE == 8
  if v > 0x7FFFFFFF then
    ret = 4
    v32 = (v >> 32).narrow[UInt32]
  //#endif

  if v32 <= 0x7F then
    return ret + 1
  if v32 <= 0x7FFF then
    return ret + 2
  if v32 <= 0x7FFFFF then
    return ret + 3
  return ret + 4
}

def GetLengthInBytesOfSInt (v: Int64): Int = {
  if v >= 0 then
    return GetLengthSIntHelper(v.toUnsigned[UInt64])

  return GetLengthSIntHelper((-v - 1).toUnsigned[UInt64])
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

def BitStream_EncodeConstraintPosWholeNumber(pBitStrm: BitStream, v: UInt64, min: UInt64, max: UInt64): Unit = {
  assert(min <= v)
  assert(v <= max)
  val range: UInt64 = (max - min)
  if range == fromInt(0) then
    return
  val nRangeBits: Int = GetNumberOfBitsForNonNegativeInteger(range)
  val nBits: Int = GetNumberOfBitsForNonNegativeInteger(v - min)
  BitStream_AppendNBitZero(pBitStrm, nRangeBits - nBits)
  BitStream_EncodeNonNegativeInteger(pBitStrm, v - min)
}

def BitStream_DecodeConstraintWholeNumber(pBitStrm: BitStream, v: Ref[Int64], min: Int64, max: Int64): Boolean = {
  val uv: Ref[UInt64] = Ref[UInt64](0)
  val range: UInt64 = (max - min).toUnsigned[UInt64]

//  ASSERT_OR_RETURN_FALSE(min <= max);

  v.x = 0
  if range == fromInt(0) then
    v.x = min
    return true

  val nRangeBits = GetNumberOfBitsForNonNegativeInteger(range)

  if BitStream_DecodeNonNegativeInteger(pBitStrm, uv, nRangeBits) then
    v.x = uv.x.toSigned[Int64] + min
    return true

  return false
}

def BitStream_DecodeConstraintWholeNumberInt8(pBitStrm: BitStream, v: Ref[Int8], min: Int8, max: Int8): Boolean = {
  val bv: Ref[Int64] = Ref[Int64](0)
  val ret: Boolean = BitStream_DecodeConstraintWholeNumber(pBitStrm, bv, min.widen[Int64], max.widen[Int64])
  v.x = bv.x.narrow[Int8]
  return ret
}

def BitStream_DecodeConstraintWholeNumberInt16(pBitStrm: BitStream, v: Ref[Int16], min: Int16, max: Int16): Boolean = {
  val bv: Ref[Int64] = Ref[Int64](0)
  val ret: Boolean = BitStream_DecodeConstraintWholeNumber(pBitStrm, bv, min.widen[Int64], max.widen[Int64])
  v.x = bv.x.narrow[Int16]
  return ret
}

def BitStream_DecodeConstraintWholeNumberInt32(pBitStrm: BitStream, v: Ref[Int32], min: Int32, max: Int32): Boolean = {
  val bv: Ref[Int64] = Ref[Int64](0)
  val ret: Boolean = BitStream_DecodeConstraintWholeNumber(pBitStrm, bv, min.widen[Int64], max.widen[Int64])
  v.x = bv.x.narrow[Int32]
  return ret
}

def BitStream_DecodeConstraintWholeNumberUInt8(pBitStrm: BitStream, v: Ref[UInt8], min: UInt8, max: UInt8): Boolean = {
  val bv: Ref[UInt64] = Ref[UInt64](0)
  val ret: Boolean = BitStream_DecodeConstraintPosWholeNumber(pBitStrm, bv, min.widen[UInt64], max.widen[UInt64])
  v.x = bv.x.narrow[UInt8]
  return ret
}

def BitStream_DecodeConstraintWholeNumberUInt16(pBitStrm: BitStream, v: Ref[UInt16], min: UInt16, max: UInt16): Boolean = {
  val bv: Ref[UInt64] = Ref[UInt64](0)
  val ret: Boolean = BitStream_DecodeConstraintPosWholeNumber(pBitStrm, bv, min.widen[UInt64], max.widen[UInt64])
  v.x = bv.x.narrow[UInt16]
  return ret
}

def BitStream_DecodeConstraintWholeNumberUInt32(pBitStrm: BitStream, v: Ref[UInt32], min: UInt32, max: UInt32): Boolean = {
  val bv: Ref[UInt64] = Ref[UInt64](0)
  val ret: Boolean = BitStream_DecodeConstraintPosWholeNumber(pBitStrm, bv, min.widen[UInt64], max.widen[UInt64])
  v.x = bv.x.narrow[UInt32]
  return ret
}


def BitStream_DecodeConstraintPosWholeNumber(pBitStrm: BitStream, v: Ref[UInt64], min: UInt64, max: UInt64): Boolean = {
  val uv: Ref[UInt64] = Ref[UInt64](0)
  val range: UInt64 = max - min

  //ASSERT_OR_RETURN_FALSE(min <= max);

  v.x = 0
  if range == fromInt(0) then
    v.x = min
    return true

  val nRangeBits: Int = GetNumberOfBitsForNonNegativeInteger(range)

  if BitStream_DecodeNonNegativeInteger(pBitStrm, uv, nRangeBits) then
    v.x = uv.x + min
    return true

  return false
}

def BitStream_EncodeSemiConstraintWholeNumber(pBitStrm: BitStream, v: Int64, min: Int64): Unit = {
  assert(v >= min)
  val nBytes: Int = GetLengthInBytesOfUInt((v - min).toUnsigned[UInt64])

  /* encode length */
  BitStream_EncodeConstraintWholeNumber(pBitStrm, fromInt(nBytes).widen[Int64], 0, 255)
  /*8 bits, first bit is always 0*/
  /* put required zeros*/
  BitStream_AppendNBitZero(pBitStrm, nBytes * 8 - GetNumberOfBitsForNonNegativeInteger((v - min).toUnsigned[UInt64]))
  /*Encode number */
  BitStream_EncodeNonNegativeInteger(pBitStrm, (v - min).toUnsigned[UInt64])
}

def BitStream_EncodeSemiConstraintPosWholeNumber(pBitStrm: BitStream, v: UInt64, min: UInt64): Unit = {
  assert(v >= min)
  val nBytes: Int = GetLengthInBytesOfUInt(v - min)

  /* encode length */
  BitStream_EncodeConstraintWholeNumber(pBitStrm, fromInt(nBytes).widen[Int64], 0, 255)
  /*8 bits, first bit is always 0*/
  /* put required zeros*/
  BitStream_AppendNBitZero(pBitStrm, nBytes * 8 - GetNumberOfBitsForNonNegativeInteger(v - min))
  /*Encode number */
  BitStream_EncodeNonNegativeInteger(pBitStrm, v - min)
}

def BitStream_DecodeSemiConstraintWholeNumber(pBitStrm:BitStream, v: Ref[Int64], min: Int64): Boolean = {
  val nBytes: Ref[Int64] = Ref[Int64](0)
  v.x = 0
  if !BitStream_DecodeConstraintWholeNumber(pBitStrm, nBytes, 0, 255) then
    return false

  var i: Int64 = 0
  while i < nBytes.x do
    decreases(nBytes.x - i)
    val b: Ref[UInt8] = Ref[UInt8](0)
    if !BitStream_ReadByte(pBitStrm, b) then
      return false
    v.x = (v.x << 8) | b.x.widen[UInt64].toSigned[Int64]
    i += 1
  v.x += min
  return true
}

def BitStream_DecodeSemiConstraintPosWholeNumber(pBitStrm:BitStream, v: Ref[UInt64], min: UInt64): Boolean = {
  val nBytes: Ref[Int64] = Ref[Int64](0)
  v.x = 0
  if !BitStream_DecodeConstraintWholeNumber(pBitStrm, nBytes, 0, 255) then
    return false

  var i: Int64 = 0
  while i < nBytes.x do
    decreases(nBytes.x - i)
    val b: Ref[UInt8] = Ref[UInt8](0)
    if !BitStream_ReadByte(pBitStrm, b) then
      return false
    v.x = (v.x << 8) | b.x.widen[UInt64]
    i += 1
  v.x += min
  return true
}

def BitStream_EncodeUnConstraintWholeNumber(pBitStrm: BitStream, v: Int64): Unit = {
  val nBytes: Int = GetLengthInBytesOfSInt(v)

  /* encode length */
  BitStream_EncodeConstraintWholeNumber(pBitStrm, fromInt(nBytes).widen[Int64], 0, 255)
  /*8 bits, first bit is always 0*/

  if v >= 0 then
    BitStream_AppendNBitZero(pBitStrm, nBytes * 8 - GetNumberOfBitsForNonNegativeInteger(v.toUnsigned[UInt64]))
    BitStream_EncodeNonNegativeInteger(pBitStrm, v.toUnsigned[UInt64])
  else
    BitStream_AppendNBitOne(pBitStrm, nBytes * 8 - GetNumberOfBitsForNonNegativeInteger((-v - 1).toUnsigned[UInt64]))
    BitStream_EncodeNonNegativeIntegerNeg(pBitStrm, (-v - 1).toUnsigned[UInt64], true)
}

def BitStream_DecodeUnConstraintWholeNumber(pBitStrm: BitStream, v: Ref[Int64]): Boolean = {
  val nBytes: Ref[Int64] = Ref[Int64](0)

  if !BitStream_DecodeConstraintWholeNumber(pBitStrm, nBytes, 0, 255) then
    return false

  val valIsNegative: Boolean = BitStream_PeekBit(pBitStrm)

  v.x = if valIsNegative then max[Int64] else 0

  var i: Int64 = 0
  while i < nBytes.x do
    decreases(nBytes.x - i)
    val b: Ref[UInt8] = Ref[UInt8](0)
    if !BitStream_ReadByte(pBitStrm, b) then
      return false
    v.x = (v.x << 8) | b.x.widen[UInt64].toSigned[Int64]
    i += 1

  return true
}

/**
Bynary encoding will be used
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
ab: F  (0..3)
cd:00 --> 1 byte for exponent as 2's complement
cd:01 --> 2 byte for exponent as 2's complement
cd:10 --> 3 byte for exponent as 2's complement
cd:11 --> 1 byte for encoding the length of the exponent, then the expoent

8 7 6 5 4 3 2 1
+-+-+-+-+-+-+-+-+
|1|S|0|0|a|b|c|d|
+-+-+-+-+-+-+-+-+
**/

//#if FP_WORD_SIZE==8
val ExpoBitMask = 0x7FF0000000000000L
val MantBitMask = 0x000FFFFFFFFFFFFFL
val MantBitMask2 = 0xFFE0000000000000L
val MantisaExtraBit = 0x0010000000000000L
//#else
//#define ExpoBitMask  0x7F800000U
//#define MantBitMask  0x007FFFFFU
//#define MantBitMask2 0xF0000000U
//#define MantisaExtraBit 0x00800000U
//#endif

/*
def CalculateMantissaAndExponent(d: Double, exponent: Ref[Int], mantissa: Ref[UInt64]): Unit = {
  //#if FP_WORD_SIZE == 8
  union {
    asn1Real in;
    asn1SccUint64 out;
  } double2uint;
  val ll: UInt64 = 0
  //#else
  //union {
  //  asn1Real in;
  //  asn1SccUint32 out;
  //} double2uint;
  //asn1SccUint32 ll = 0;
  //#endif

  double2uint.in = d;
  ll = double2uint.out;

  exponent.x = 0
  mantissa.x = 0

  //#if FP_WORD_SIZE == 8
  exponent.x = (((ll & ExpoBitMask) >> 52) - 1023 - 52).toInt
  mantissa.x = ll & MantBitMask
  mantissa.x |= MantisaExtraBit
  //#else
  //exponent.x = (int)(((ll & ExpoBitMask) >> 23) - 127 - 23);
  //mantissa.x = ll & MantBitMask;
  //mantissa.x |= MantisaExtraBit;
  //#endif
}
 */

/*
def GetDoubleByMantissaAndExp(mantissa: UInt64, exponentVal: Int): Double = {
  var exponent = exponentVal
  var ret: Double = 1.0
  if mantissa == 0 then
    return 0.0

  if exponent >= 0 then
    while exponent >= 0 do
      ret = ret * 2.0
      exponent -= 1
    return mantissa * ret

  else
    exponent = -exponent
    while exponent > 0 do
      ret = ret * 2.0
      exponent -= 1
    return mantissa / ret
}
 */

/*
def BitStream_EncodeReal(pBitStrm: BitStream, vVal: Double): Unit = {
  var header: UInt8 = 0x80

  var v = vVal
  if v == 0.0 then
    BitStream_EncodeConstraintWholeNumber(pBitStrm, 0, 0, 0xFF)
    return

  if v == Double.PositiveInfinity then
    BitStream_EncodeConstraintWholeNumber(pBitStrm, 1, 0, 0xFF)
    BitStream_EncodeConstraintWholeNumber(pBitStrm, 0x40, 0, 0xFF)
    return

  if v == Double.NegativeInfinity then
    BitStream_EncodeConstraintWholeNumber(pBitStrm, 1, 0, 0xFF)
    BitStream_EncodeConstraintWholeNumber(pBitStrm, 0x41, 0, 0xFF)
    return

  if v < 0 then
    header |= 0x40
    v = -v

  val exponent: Ref[UInt64] = Ref[UInt64](0)
  val mantissa: Ref[UInt64] = Ref[UInt64](0)

  CalculateMantissaAndExponent(v, exponent, mantissa)
  val nExpLen: Int = GetLengthInBytesOfSInt(exponent.x.toSigned[Int64])
  val nManLen: Int = GetLengthInBytesOfUInt(mantissa.x)
  assert(nExpLen <= 3)
  if nExpLen == 2 then
    header |= 1
  else if nExpLen == 3 then
    header |= 2

  /* encode length */
  BitStream_EncodeConstraintWholeNumber(pBitStrm, fromInt(1 + nExpLen + nManLen).widen[Int64], 0, 0xFF)

  /* encode header */
  BitStream_EncodeConstraintWholeNumber(pBitStrm, header.widen[UInt64].toSigned[Int64], 0, 0xFF)

  /* encode exponent */
  if exponent.x >= 0 then
    BitStream_AppendNBitZero(pBitStrm, nExpLen * 8 - GetNumberOfBitsForNonNegativeInteger(exponent.x))
    BitStream_EncodeNonNegativeInteger(pBitStrm, exponent.x)
  else
    BitStream_AppendNBitOne(pBitStrm, nExpLen * 8 - GetNumberOfBitsForNonNegativeInteger(-exponent.x - 1))
    BitStream_EncodeNonNegativeIntegerNeg(pBitStrm, -exponent.x - 1, true)

  /* encode mantissa */
  BitStream_AppendNBitZero(pBitStrm, nManLen * 8 - GetNumberOfBitsForNonNegativeInteger(mantissa.x))
  BitStream_EncodeNonNegativeInteger(pBitStrm, mantissa.x)
}
*/

/*
def BitStream_DecodeReal(pBitStrm: BitStream, v: Ref[Double]): Boolean = {
  val header: Ref[UInt8] = Ref[UInt8](0)
  val length: Ref[UInt8] = Ref[UInt8](0)

  if !BitStream_ReadByte(pBitStrm, length) then
    return false

  if length.x == fromInt(0) then
    v.x = 0.0
    return true

  if !BitStream_ReadByte(pBitStrm, header) then
    return false

  if header.x == fromInt(0x40) then
    v.x = Double.PositiveInfinity;
    return true

  if header.x == fromInt(0x41) then
    v.x = Double.NegativeInfinity
    return true

  return DecodeRealAsBinaryEncoding(pBitStrm, length.x.toInt - 1, header.x, v)
}
*/

/*
def DecodeRealAsBinaryEncoding(pBitStrm: BitStream, lengthVal: Int, header: UInt8, v: Ref[Double]): Boolean = {
  var length = lengthVal
  var sign: Int = 1
  /*int base=2;*/
  var factor: UInt64 = 1
  var expFactor: Int = 1
  var N: UInt64 = 0

  if (header & 0x40) > 0 then
    sign = -1
  if (header & 0x10) > 0 then
    /*base = 8;*/
    expFactor = 3
  else if (header & 0x20) > 0 then
    /*base = 16;*/
    expFactor = 4

  val F: Int = ((header & 0x0C) >> 2).toInt
  factor <<= F

  val expLen: Int = ((header & 0x03) + 1).toInt

  if expLen > length then
    return false

  val expIsNegative = BitStream_PeekBit(pBitStrm)
  var exponent: Int = if expIsNegative then 0xFFFFFFFF else 0

  var i: Int = 0
  while i < expLen do
    decreases(expLen - i)
    val b: Ref[UInt8] = Ref[UInt8](0)
    if !BitStream_ReadByte(pBitStrm, b) then
      return false
    exponent = exponent << 8 | b.x.toInt
    i += 1

  length -= expLen

  var j: Int = 0
  while j < length do
    decreases(length - j)
    val b: Ref[UInt8] = Ref[UInt8](0)
    if !BitStream_ReadByte(pBitStrm, b) then
      return false
    N = N << 8 | b.x.widen[UInt64]
    j += 1

  /*  *v = N*factor * pow(base,exp);*/
  v.x = GetDoubleByMantissaAndExp(N * factor, expFactor * exponent)

  if sign < 0 then
    v.x = -v.x

  return true
}
*/

def BitStream_checkBitPatternPresent(pBitStrm: BitStream, bit_terminated_pattern: Array[UInt8], bit_terminated_pattern_size_in_bitsVal: UInt8): Int = {
  var bit_terminated_pattern_size_in_bits = bit_terminated_pattern_size_in_bitsVal
  val tmp_currentByte: Int = pBitStrm.currentByte
  val tmp_currentBit: Int = pBitStrm.currentBit
  val tmp_byte: Ref[UInt8] = Ref[UInt8](0)

  if pBitStrm.currentByte * 8 + pBitStrm.currentBit + bit_terminated_pattern_size_in_bits.toInt > pBitStrm.buf.length * 8 then
    return 0

  var i: Int = 0
  while bit_terminated_pattern_size_in_bits >= 8 do
    decreases(bit_terminated_pattern_size_in_bits)
    if !BitStream_ReadByte(pBitStrm, tmp_byte) then
      return 0
    bit_terminated_pattern_size_in_bits = 8
    if bit_terminated_pattern(i) != tmp_byte.x then
      pBitStrm.currentByte = tmp_currentByte
      pBitStrm.currentBit = tmp_currentBit
      return 1
    i += 1

  if bit_terminated_pattern_size_in_bits > 0 then
    if !BitStream_ReadPartialByte(pBitStrm, tmp_byte, bit_terminated_pattern_size_in_bits) then
      return 0
    tmp_byte.x = (tmp_byte.x << (8 - bit_terminated_pattern_size_in_bits))

    if bit_terminated_pattern(i) != tmp_byte.x then
      pBitStrm.currentByte = tmp_currentByte
      pBitStrm.currentBit = tmp_currentBit
      return 1

  return 2
}

/*
def BitStream_ReadBits_nullterminated(pBitStrm: BitStream, bit_terminated_pattern: Array[UInt8], bit_terminated_pattern_size_in_bits: UInt8, BuffToWrite: Array[UInt8], nMaxReadBits: Int, bitsRead: Ref[Int]): Boolean = {
  var checkBitPatternPresentResult: Int = 0
  bitsRead.x = 0
  var ret: Boolean = true
  val bitVal: Ref[Boolean] = Ref[Boolean](false)

  //val tmpBuf: Array[UInt8] = Array.fill(if nMaxReadBits % 8 == 0 then nMaxReadBits / 8 else nMaxReadBits / 8 + 1)(0)
  val tmpStrm: BitStream = BitStream(BuffToWrite, 0, 0)

  checkBitPatternPresentResult = BitStream_checkBitPatternPresent(pBitStrm, bit_terminated_pattern, bit_terminated_pattern_size_in_bits)
  while ret && (bitsRead.x < nMaxReadBits) && (checkBitPatternPresentResult == 1) do
    decreases(nMaxReadBits - bitsRead.x)
    ret = BitStream_ReadBit(pBitStrm, bitVal)
    if ret then
      BitStream_AppendBit(tmpStrm, bitVal.x)
      bitsRead.x += 1

    if ret && (bitsRead.x < nMaxReadBits) then
      checkBitPatternPresentResult = BitStream_checkBitPatternPresent(pBitStrm, bit_terminated_pattern, bit_terminated_pattern_size_in_bits)

  if ret && (bitsRead.x == nMaxReadBits) && (checkBitPatternPresentResult == 1) then
    checkBitPatternPresentResult = BitStream_checkBitPatternPresent(pBitStrm, bit_terminated_pattern, bit_terminated_pattern_size_in_bits)

  return ret && (checkBitPatternPresentResult == 2)
}
*/


def BitStream_EncodeOctetString_no_length(pBitStrm: BitStream, arr: Array[UInt8], nCount: Int): Boolean = {
  val cb = pBitStrm.currentBit
  //int i1;
  var ret: Boolean = true

  if cb == 0 then
    //#ifdef ASN1SCC_STREAMING
    var remainingBytesToSend: Int = nCount
    while remainingBytesToSend > 0 do
      decreases(remainingBytesToSend)
      val currentBatch =
        if pBitStrm.currentByte + remainingBytesToSend <= pBitStrm.buf.length then
          remainingBytesToSend
        else
          pBitStrm.buf.length - pBitStrm.currentByte

      //memcpy(pBitStrm.buf(pBitStrm.currentByte), arr, currentBatch) // TODO
      pBitStrm.currentByte += currentBatch
      bitstream_push_data_if_required(pBitStrm)
      remainingBytesToSend -= currentBatch

    //else
    //ret = pBitStrm.currentByte + nCount <= pBitStrm.count
    //if ret then
    //  memcpy(pBitStrm.buf(pBitStrm.currentByte), arr, nCount)
    //  pBitStrm.currentByte += nCount
    //#endif

  else
    ret = BitStream_AppendByteArray(pBitStrm, arr)
    /*
        for (i1 = 0; (i1 < (int)nCount) && ret; i1++)
        {
          ret = BitStream_AppendByte0(pBitStrm, arr[i1]);
        }
    */

  return ret
}


def BitStream_DecodeOctetString_no_length(pBitStrm: BitStream, arr: Array[UInt8], nCount: Int): Boolean = {
  val cb: Int = pBitStrm.currentBit
  //int i1;
  var ret: Boolean = true

  if cb == 0 then
    //#ifdef ASN1SCC_STREAMING
    var remainingBytesToRead: Int = nCount
    while remainingBytesToRead > 0 do
      decreases(remainingBytesToRead)
      val currentBatch: Int =
        if pBitStrm.currentByte + remainingBytesToRead <= pBitStrm.buf.length then
          remainingBytesToRead else
          pBitStrm.buf.length - pBitStrm.currentByte

      //memcpy(arr, pBitStrm.buf(pBitStrm.currentByte), currentBatch) // TODO
      pBitStrm.currentByte += currentBatch
      bitstream_fetch_data_if_required(pBitStrm)
      remainingBytesToRead -= currentBatch

    //#else
    //ret = pBitStrm.currentByte + nCount <= pBitStrm.count
    //if ret then
    //  memcpy(arr, pBitStrm.buf(pBitStrm.currentByte), nCount)
    //  pBitStrm.currentByte += nCount
    //#endif

  else
    ret = BitStream_ReadByteArray(pBitStrm, arr)
    //for (i1 = 0; (i1 < nCount) && ret; i1++)
    //{
    //    ret = BitStream_ReadByte(pBitStrm, &arr[i1]);
    //}

  return ret
}


def BitStream_EncodeOctetString_fragmentation(pBitStrm: BitStream, arr: Array[UInt8], nCount: Int): Boolean = {
  var nRemainingItemsVar1: Int = nCount
  var nCurBlockSize1: Int = 0
  var nCurOffset1: Int = 0
  var ret: Boolean = nCount >= 0

  while nRemainingItemsVar1 >= 0x4000 && ret do
    decreases(nRemainingItemsVar1)
    if nRemainingItemsVar1 >= 0x10000 then
      nCurBlockSize1 = 0x10000
      BitStream_EncodeConstraintWholeNumber(pBitStrm, 0xC4, 0, 0xFF)
    else if nRemainingItemsVar1 >= 0xC000 then
      nCurBlockSize1 = 0xC000
      BitStream_EncodeConstraintWholeNumber(pBitStrm, 0xC3, 0, 0xFF)
    else if nRemainingItemsVar1 >= 0x8000 then
      nCurBlockSize1 = 0x8000
      BitStream_EncodeConstraintWholeNumber(pBitStrm, 0xC2, 0, 0xFF)
    else
      nCurBlockSize1 = 0x4000
      BitStream_EncodeConstraintWholeNumber(pBitStrm, 0xC1, 0, 0xFF)

    var i1: Int = nCurOffset1
    while i1 < nCurBlockSize1 + nCurOffset1 && ret do
      decreases(nCurBlockSize1 + nCurOffset1 - i1)
      ret = BitStream_AppendByte0(pBitStrm, arr(i1))
      i1 += 1

    nCurOffset1 += nCurBlockSize1
    nRemainingItemsVar1 -= nCurBlockSize1

  if ret then
    if nRemainingItemsVar1 <= 0x7F then
      BitStream_EncodeConstraintWholeNumber(pBitStrm, fromInt(nRemainingItemsVar1).widen[Int64], 0, 0xFF)
    else
      BitStream_AppendBit(pBitStrm, true)
      BitStream_EncodeConstraintWholeNumber(pBitStrm, fromInt(nRemainingItemsVar1).widen[Int64], 0, 0x7FFF)


    var i1: Int = nCurOffset1
    while i1 < (nCurOffset1 + nRemainingItemsVar1) && ret do
      decreases(nCurOffset1 + nRemainingItemsVar1 - i1)
      ret = BitStream_AppendByte0(pBitStrm, arr(i1))
      i1 += 1

  return ret
}

/*
def BitStream_DecodeOctetString_fragmentation(pBitStrm: BitStream, arr: Array[UInt8], nCount: Ref[Int], asn1SizeMax: Int64): Boolean = {
  var ret: Boolean = true

  var nLengthTmp1: Int64 = 0
  val nRemainingItemsVar1: Ref[Int64] = Ref[Int64](0)
  var nCurBlockSize1: Int64 = 0
  var nCurOffset1: Int64 = 0

  ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, nRemainingItemsVar1, 0, 0xFF)
  if ret then
    while ret && (nRemainingItemsVar1.x & 0xC0) == fromInt(0xC0) do
      decreases() // TODO
      if nRemainingItemsVar1.x == fromInt(0xC4) then
        nCurBlockSize1 = 0x10000
      else if nRemainingItemsVar1.x == fromInt(0xC3) then
        nCurBlockSize1 = 0xC000
      else if nRemainingItemsVar1.x == fromInt(0xC2) then
        nCurBlockSize1 = 0x8000
      else if nRemainingItemsVar1.x == fromInt(0xC1) then
        nCurBlockSize1 = 0x4000
      else
        ret = false

      if ret then
        ret = nCurOffset1 + nCurBlockSize1 <= asn1SizeMax
        var i1: Int = nCurOffset1.toInt
        while ret && (i1 < (nCurOffset1 + nCurBlockSize1).toInt) do
          decreases((nCurOffset1 + nCurBlockSize1).toInt - i1)
          val t = Ref[UInt8](0)
          ret = BitStream_ReadByte(pBitStrm, t)
          arr(i1) = t.x
          i1 += 1

        if ret then
          nLengthTmp1 += nCurBlockSize1
          nCurOffset1 += nCurBlockSize1
          ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, nRemainingItemsVar1, 0, 0xFF)

    if ret then
      if (nRemainingItemsVar1.x & 0x80) > 0 then
        val len2 = Ref[Int64](0)
        nRemainingItemsVar1.x <<= 8
        ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, len2, 0, 0xFF)
        if ret then
          nRemainingItemsVar1.x |= len2.x
          nRemainingItemsVar1.x &= 0x7FFF

      ret = ret && (nCurOffset1 + nRemainingItemsVar1.x <= asn1SizeMax)
      if ret then
        var i1: Int = nCurOffset1.toInt
        while ret && (i1 < (nCurOffset1 + nRemainingItemsVar1.x).toInt) do
          decreases((nCurOffset1 + nRemainingItemsVar1.x).toInt - i1)
          val t = Ref[UInt8](0)
          ret = BitStream_ReadByte(pBitStrm, t)
          arr(i1) = t.x
          i1 += 1

        if ret then
          nLengthTmp1 += nRemainingItemsVar1.x

          if (nLengthTmp1 >= 1) && (nLengthTmp1 <= asn1SizeMax) then
            nCount.x = nLengthTmp1.toInt
          else
            ret = false
            /*COVERAGE_IGNORE*/

  return ret
}
*/

/*
flag BitStream_EncodeOctetString (BitStream * pBitStrm, const byte * arr, int nCount, asn1SccSint asn1SizeMin, asn1SccSint asn1SizeMax) {
  flag ret = (nCount >= asn1SizeMin && nCount <= asn1SizeMax);

  if (ret) {
    if (asn1SizeMax < 65536) {
      if (asn1SizeMin != asn1SizeMax) {
        BitStream_EncodeConstraintWholeNumber(pBitStrm, nCount, asn1SizeMin, asn1SizeMax);
      }
      ret = BitStream_EncodeOctetString_no_length(pBitStrm, arr, nCount);
    }
    else {
      ret = BitStream_EncodeOctetString_fragmentation(pBitStrm, arr, nCount);
    }
  }


  return ret;
}*/

/*
flag BitStream_DecodeOctetString (BitStream * pBitStrm, byte * arr, int * nCount, asn1SccSint asn1SizeMin, asn1SccSint asn1SizeMax) {
  flag ret = TRUE;
  if (asn1SizeMax < 65536) {
    asn1SccSint nCountL;
    if (asn1SizeMin != asn1SizeMax) {
      ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, & nCountL, asn1SizeMin, asn1SizeMax);
    }
    else {
      ret = TRUE;
      nCountL = asn1SizeMin;
    }
    * nCount = (int) nCountL;
    ret = ret && (nCountL >= asn1SizeMin && nCountL <= asn1SizeMax);
    if (ret) {
      BitStream_DecodeOctetString_no_length(pBitStrm, arr, * nCount);
    }
  }
  else {
    ret = BitStream_DecodeOctetString_fragmentation(pBitStrm, arr, nCount, asn1SizeMax);
  }
  return ret;
}*/


/*
flag BitStream_EncodeBitString (BitStream * pBitStrm, const byte * arr, int nCount, asn1SccSint asn1SizeMin, asn1SccSint asn1SizeMax) {
  if (asn1SizeMax < 65536) {
    if (asn1SizeMin != asn1SizeMax) {
      BitStream_EncodeConstraintWholeNumber(pBitStrm, nCount, asn1SizeMin, asn1SizeMax);
    }
    BitStream_AppendBits(pBitStrm, arr, nCount);
  }
  else {
    asn1SccSint nRemainingItemsVar1;
    asn1SccSint nCurBlockSize1;
    asn1SccSint nCurOffset1;
    nRemainingItemsVar1 = nCount;
    nCurBlockSize1 = 0;
    nCurOffset1 = 0;
    while (nRemainingItemsVar1 >= 0x4000) {
      if (nRemainingItemsVar1 >= 0x10000) {
        nCurBlockSize1 = 0x10000;
        BitStream_EncodeConstraintWholeNumber(pBitStrm, 0xC4, 0, 0xFF);
      }
      else if (nRemainingItemsVar1 >= 0xC000) {
        nCurBlockSize1 = 0xC000;
        BitStream_EncodeConstraintWholeNumber(pBitStrm, 0xC3, 0, 0xFF);
      }
      else if (nRemainingItemsVar1 >= 0x8000) {
        nCurBlockSize1 = 0x8000;
        BitStream_EncodeConstraintWholeNumber(pBitStrm, 0xC2, 0, 0xFF);
      }
      else {
        nCurBlockSize1 = 0x4000;
        BitStream_EncodeConstraintWholeNumber(pBitStrm, 0xC1, 0, 0xFF);
      }

      BitStream_AppendBits(pBitStrm, & arr[nCurOffset1 / 8]
      , (int) nCurBlockSize1
      );
      nCurOffset1 += nCurBlockSize1;
      nRemainingItemsVar1 -= nCurBlockSize1;
    }

    if (nRemainingItemsVar1 <= 0x7F)
      BitStream_EncodeConstraintWholeNumber(pBitStrm, nRemainingItemsVar1, 0, 0xFF);
    else {
      BitStream_AppendBit(pBitStrm, 1);
      BitStream_EncodeConstraintWholeNumber(pBitStrm, nRemainingItemsVar1, 0, 0x7FFF);
    }

    BitStream_AppendBits(pBitStrm, & arr[nCurOffset1 / 8]
    , (int) nRemainingItemsVar1
    );
  }

  return TRUE;

}*/

/*
flag BitStream_DecodeBitString (BitStream * pBitStrm, byte * arr, int * pCount, asn1SccSint asn1SizeMin, asn1SccSint asn1SizeMax) {
  flag ret = TRUE;
  if (asn1SizeMax < 65536) {
    asn1SccSint nCount;
    if (asn1SizeMin != asn1SizeMax) {
      ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, & nCount, asn1SizeMin, asn1SizeMax);
    }
    else {
      nCount = asn1SizeMin;
    }
    if (ret) {
      * pCount = (int) nCount;
      ret = BitStream_ReadBits(pBitStrm, arr, * pCount);
    }
  }
  else {
    asn1SccSint nLengthTmp1;
    asn1SccSint nRemainingItemsVar1;
    asn1SccSint nCurBlockSize1;
    asn1SccSint nCurOffset1;

    nRemainingItemsVar1 = 0;
    nCurBlockSize1 = 0;
    nCurOffset1 = 0;
    nLengthTmp1 = 0;
    ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, & nRemainingItemsVar1, 0, 0xFF);
    if (ret) {
      while (ret && (nRemainingItemsVar1 & 0xC0) == 0xC0) {
        if (nRemainingItemsVar1 == 0xC4)
          nCurBlockSize1 = 0x10000;
        else if (nRemainingItemsVar1 == 0xC3)
          nCurBlockSize1 = 0xC000;
        else if (nRemainingItemsVar1 == 0xC2)
          nCurBlockSize1 = 0x8000;
        else if (nRemainingItemsVar1 == 0xC1)
          nCurBlockSize1 = 0x4000;
        else {
          return FALSE;
          /*COVERAGE_IGNORE*/
        }
        if (nCurOffset1 + nCurBlockSize1 > asn1SizeMax) {
          return FALSE;
          /*COVERAGE_IGNORE*/
        }

        ret = BitStream_ReadBits(pBitStrm, & arr[nCurOffset1 / 8]
        , (int) nCurBlockSize1
        );

        if (ret) {
          nLengthTmp1 += (long) nCurBlockSize1;
          nCurOffset1 += nCurBlockSize1;
          ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, & nRemainingItemsVar1, 0, 0xFF);
        }
      }
      if (ret) {
        if ((nRemainingItemsVar1 & 0x80) > 0) {
          asn1SccSint len2;
          nRemainingItemsVar1 <<= 8;
          ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, & len2, 0, 0xFF);
          if (ret) {
            nRemainingItemsVar1 |= len2;
            nRemainingItemsVar1 &= 0x7FFF;
          }
        }
        ret = ret && (nCurOffset1 + nRemainingItemsVar1 <= asn1SizeMax);
        if (ret) {
          ret = BitStream_ReadBits(pBitStrm, & arr[nCurOffset1 / 8]
          , (int) nRemainingItemsVar1
          );
          if (ret) {
            nLengthTmp1 += (long) nRemainingItemsVar1;

            if ((nLengthTmp1 >= 1) && (nLengthTmp1 <= asn1SizeMax)) {
              * pCount = (int) nLengthTmp1;
            } else {
              ret = FALSE;
              /*COVERAGE_IGNORE*/
            }

          }
        }
      }
    }
  }
  return ret;
}*/

// TODO: use pStrm.fetchDataPrm, pStrm.pushDataPrm
def fetchData(pBitStrm: BitStream) = ???
def pushData(pBitStrm: BitStream) = ???

//def fetchData(pBitStrm: BitStream, param: Any) = ???
//def pushData(pBitStrm: BitStream, param: Any) = ???

def bitstream_fetch_data_if_required(pStrm: BitStream): Unit = {
  if pStrm.currentByte == pStrm.buf.length then
  //if pStrm.currentByte == pStrm.buf.length && pStrm.fetchDataPrm != null then
    //fetchData(pStrm, pStrm.fetchDataPrm)
    fetchData(pStrm)
    pStrm.currentByte = 0
}
def bitstream_push_data_if_required(pStrm: BitStream): Unit = {
  if pStrm.currentByte == pStrm.buf.length then
  //if pStrm.currentByte == pStrm.buf.length && pStrm.pushDataPrm != null then
    //pushData(pStrm, pStrm.pushDataPrm)
    pushData(pStrm)
    pStrm.currentByte = 0
}
