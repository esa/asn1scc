package asn1scala

import stainless.lang.{None => _, Option => _, Some => _, _}

val masks: Array[UByte] = Array(
    -0x80, // -128 / 1000 0000 / x80
     0x40, //     64 / 0100 0000 / x40
     0x20, //     32 / 0010 0000 / x20
     0x10, //     16 / 0001 0000 / x10
     0x08, //        8 / 0000 1000 / x08
     0x04, //        4 / 0000 0100 / x04
     0x02, //        2 / 0000 0010 / x02
     0x01, //        1 / 0000 0001 / x01
)
val masksb: Array[UByte] = Array(
    0x00, //     0 / 0000 0000 / x00
    0x01, //     1 / 0000 0001 / x01
    0x03, //     3 / 0000 0011 / x03
    0x07, //     7 / 0000 0111 / x07
    0x0F, //    15 / 0000 1111 / x0F
    0x1F, //    31 / 0001 1111 / x1F
    0x3F, //    63 / 0011 1111 / x3F
    0x7F, // 127 / 0111 1111 / x7F
    -0x1, //    -1 / 1111 1111 / xFF
)
val masks2: Array[UInt] = Array(
    0x00000000, //                 0 / 0000 0000 0000 0000 0000 0000 0000 0000 / 0x00000000
    0x000000FF, //             255 / 0000 0000 0000 0000 0000 0000 1111 1111 / 0x000000FF
    0x0000FF00, //         65280 / 0000 0000 0000 0000 1111 1111 0000 0000 / 0x0000FF00
    0x00FF0000, //    16711680 / 0000 0000 1111 1111 0000 0000 0000 0000 / 0x00FF0000
    0xFF000000, // -16777216 / 1111 1111 0000 0000 0000 0000 0000 0000 / 0xFF000000
)


/***********************************************************************************************/
/**    Byte Stream Functions                                                                                                                                        **/
/***********************************************************************************************/
def ByteStream_Init(count: Int): ByteStream = {
    ByteStream(Array.fill(count)(0), 0, false)
}

def ByteStream_AttachBuffer(pStrm: ByteStream, buf: Array[UByte]): Unit = {
    pStrm.buf = buf // TODO: fix illegal aliasing
    pStrm.currentByte = 0
}

def ByteStream_GetLength(pStrm: ByteStream): Int = {
    pStrm.currentByte
}

/***********************************************************************************************/
/**    Bit Stream Functions                                                                                                                                         **/
/***********************************************************************************************/
def BitString_equal(arr1: Array[UByte], arr2: Array[UByte]): Boolean = {
    arr1.sameElements(arr2)
    //return
    //    (nBitsLength1 == nBitsLength2) &&
    //        (nBitsLength1 / 8 == 0 || memcmp(arr1, arr2, nBitsLength1 / 8) == 0) &&
    //        (nBitsLength1 % 8 > 0 ? (arr1[nBitsLength1 / 8] >>> (8 - nBitsLength1 % 8) == arr2[nBitsLength1 / 8] >>> (8 - nBitsLength1 % 8)): TRUE);
}


def BitStream_Init(count: Int): BitStream = {
    BitStream(Array.fill(count)(0), 0, 0, None, None)
}

def BitStream_Init2(count: Int, fetchDataPrm: Option[Any], pushDataPrm: Option[Any]): BitStream = {
    BitStream(Array.fill(count)(0), 0, 0, fetchDataPrm, pushDataPrm)
}


def BitStream_AttachBuffer(pBitStrm: BitStream, buf: Array[UByte]): Unit = {
    pBitStrm.buf = buf // TODO: fix illegal aliasing
    pBitStrm.currentByte = 0
    pBitStrm.currentBit = 0
    pBitStrm.pushDataPrm = None
    pBitStrm.fetchDataPrm = None
}

def BitStream_AttachBuffer2(pBitStrm: BitStream, buf: Array[UByte], fetchDataPrm: Option[Any], pushDataPrm: Option[Any]): Unit = {
    BitStream_AttachBuffer(pBitStrm, buf)
    pBitStrm.pushDataPrm = pushDataPrm
    pBitStrm.fetchDataPrm = fetchDataPrm
}

def BitStream_GetLength(pBitStrm: BitStream): Int = {
    var ret: Int = pBitStrm.currentByte
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
or    00010000
------------
        xxx1????
**/

def BitStream_AppendBitOne(pBitStrm: BitStream): Unit = {
    pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | masks(pBitStrm.currentBit)).toByte

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
    pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & nmask).toByte
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
        BitStream_AppendByte(pBitStrm, 0xFF.toByte, false)
        nbits -= 8

    var i = 0
    while i < nbits do
        decreases(nbits - i)
        BitStream_AppendBitOne(pBitStrm)
        i+= 1
}

def BitStream_AppendBits(pBitStrm: BitStream, srcBuffer: Array[UByte], nbits: Int): Unit = {
    var lastByte: UByte = 0

    val bytesToEncode: Int = nbits / 8
    val remainingBits: UByte = (nbits % 8).toByte

    BitStream_EncodeOctetString_no_length(pBitStrm, srcBuffer, bytesToEncode)

    if remainingBits > 0 then
        lastByte = (srcBuffer(bytesToEncode) >>> (8 - remainingBits)).toByte
        BitStream_AppendPartialByte(pBitStrm, lastByte, remainingBits, false)
}

def BitStream_AppendBit(pBitStrm: BitStream, v: Boolean): Unit = {
    // TODO: currentByte=pBitStrm.buf.length temp possible, but with bitstream_push_data_if_required set to 0 again
    require(pBitStrm.currentByte + (pBitStrm.currentBit+1) / 8 < pBitStrm.buf.length)
    if v then
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | masks(pBitStrm.currentBit)).toByte
    else
        val nmask = ~masks(pBitStrm.currentBit)
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & nmask).toByte

    if pBitStrm.currentBit < 7 then
        pBitStrm.currentBit += 1
    else
        pBitStrm.currentBit = 0
        pBitStrm.currentByte += 1
        bitstream_push_data_if_required(pBitStrm)

    assert(pBitStrm.currentByte + pBitStrm.currentBit/8 <= pBitStrm.buf.length)
}

def BitStream_ReadBit(pBitStrm: BitStream): Option[Boolean] = {
    val ret = (pBitStrm.buf(pBitStrm.currentByte) & masks(pBitStrm.currentBit)) != 0

    if pBitStrm.currentBit < 7 then
        pBitStrm.currentBit += 1
    else
        pBitStrm.currentBit = 0
        pBitStrm.currentByte += 1
        bitstream_fetch_data_if_required(pBitStrm)

    if pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.buf.length * 8 then
        Some(ret)
    else
        None
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
or    000bbbbb
------------
        xxxbbbbb

**/
def BitStream_AppendByte(pBitStrm: BitStream, vVal: UByte, negate: Boolean): Unit = {
    //static UByte masksb[] = { 0x0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF };
    val cb: UByte = pBitStrm.currentBit.toByte
    val ncb: UByte = (8 - cb).toByte

    var v = vVal
    if negate then
        v = (~v).toByte

    var mask: UByte = (~masksb(ncb)).toByte

    pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask).toByte
    pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | ((v & 0xFF ) >>> cb)).toByte
    pBitStrm.currentByte += 1
    bitstream_push_data_if_required(pBitStrm)

    assert(pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.buf.length * 8)

    if cb > 0 then
        mask = (~mask).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v << ncb)).toByte // TODO: check if & 0xFF is needed
}

def BitStream_AppendByte0(pBitStrm: BitStream, v: UByte): Boolean = {
    val cb: UByte = pBitStrm.currentBit.toByte
    val ncb: UByte = (8-cb).toByte

    var mask = ~masks(ncb)

    pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask).toByte
    pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v >>> cb)).toByte
    pBitStrm.currentByte += 1
    bitstream_push_data_if_required(pBitStrm)

    if cb > 0 then
        if pBitStrm.currentByte >= pBitStrm.buf.length then
            return false
        mask = ~mask
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v << cb)).toByte

    true
}

def BitStream_AppendByteArray(pBitStrm: BitStream, arr: Array[UByte], arr_len: Int): Boolean = {
    //static byte    masks[] = { 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 };
    //static byte masksb[] = { 0x00, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F, 0xFF };

    val cb: UByte = pBitStrm.currentBit.toByte
    val ncb: UByte = (8 - cb).toByte

    val mask: UByte = (~masksb(ncb)).toByte
    val nmask: UByte = (~mask).toByte

    //if (pBitStrm->currentByte + (int)arr_len + (cb > 0 ? 1 : 0) >= pBitStrm->count)
    if (pBitStrm.currentByte+arr_len)*8+pBitStrm.currentBit > pBitStrm.buf.length*8 then
        return false

    if arr_len > 0 then
        val v: UByte = arr(0)
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask).toByte     //make zero right bits (i.e. the ones that will get the new value)
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v >>> cb)).toByte    //shift right and then populate current byte
        pBitStrm.currentByte += 1
        bitstream_push_data_if_required(pBitStrm)

        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & nmask).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v << ncb)).toByte

    var i: Int = 1
    while i < arr_len-1 do
        decreases(arr_len-1-i)
        val v: UByte = arr(i)
        val v1: UByte = (v >>> cb).toByte
        val v2: UByte = (v << ncb).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | v1).toByte //shift right and then populate current byte
        pBitStrm.currentByte += 1
        bitstream_push_data_if_required(pBitStrm)
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | v2).toByte
        i += 1

    if arr_len - 1 > 0 then
        val v: UByte = arr(arr_len - 1)
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask ).toByte            //make zero right bits (i.e. the ones that will get the new value)
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v >>> cb)).toByte    //shift right and then populate current byte
        pBitStrm.currentByte += 1
        bitstream_push_data_if_required(pBitStrm)

        if cb > 0 then
            pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & nmask).toByte
            pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v << ncb)).toByte

    true
}

def BitStream_ReadByte(pBitStrm: BitStream): Option[UByte] = {

    val cb: UByte = pBitStrm.currentBit.toByte
    val ncb: UByte = (8 - cb).toByte

    var v: UByte = (pBitStrm.buf(pBitStrm.currentByte) << cb).toByte
    pBitStrm.currentByte += 1
    bitstream_fetch_data_if_required(pBitStrm)

    if cb > 0 then
        v = (v | pBitStrm.buf(pBitStrm.currentByte) >>> ncb).toByte // TODO: check if & 0xFF is needed

    if pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.buf.length * 8 then
        Some(v)
    else
        None
}

def BitStream_ReadByteArray(pBitStrm: BitStream, arr_len: Int): Option[Array[UByte]] = {
    val arr: Array[UByte] = Array.fill(arr_len+1)(0)

    val cb: UByte = pBitStrm.currentBit.toByte
    val ncb: UByte = (8 - cb).toByte

    if (pBitStrm.currentByte+arr_len)*8+cb.toInt > pBitStrm.buf.length*8 then
        return None

    var i: Int = 0
    while i < arr_len do
        decreases(arr_len - i)
        arr(i) = (pBitStrm.buf(pBitStrm.currentByte) << cb).toByte
        pBitStrm.currentByte += 1
        bitstream_fetch_data_if_required(pBitStrm)
        arr(i) = (arr(i) | (pBitStrm.buf(pBitStrm.currentByte) & 0xFF) >>> ncb).toByte
        i += 1

    Some(arr)
}

def BitStream_ReadBits(pBitStrm: BitStream, nbits: Int): Option[Array[UByte]] = {
    val bytesToRead: Int = nbits / 8
    val remainingBits: UByte = (nbits % 8).toByte
    var ret: Boolean = false

    BitStream_DecodeOctetString_no_length(pBitStrm, bytesToRead) match
        case None => return None
        case Some(arr) =>
            if remainingBits > 0 then
                BitStream_ReadPartialByte(pBitStrm, remainingBits) match
                    case None => None
                    case Some(ub) => arr(bytesToRead) = ub
                arr(bytesToRead) = (arr(bytesToRead) << (8 - remainingBits)).toByte
                Some(arr)
            else
                Some(arr)
}


/* nbits 1..7*/
def BitStream_AppendPartialByte(pBitStrm: BitStream, vVal: UByte, nbits: UByte, negate: Boolean): Unit = {
    val cb: UByte = pBitStrm.currentBit.toByte
    val totalBits: UByte = (cb + nbits).toByte
    val ncb: UByte = (8 - cb).toByte

    var v = vVal
    if negate then
        v = (masksb(nbits) & (~v)).toByte

    val mask1: UByte = (~masksb(ncb)).toByte

    if (totalBits <= 8) {
        //static UByte masksb[] = { 0x0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF };
        val mask2: UByte = masksb(8 - totalBits)
        val mask: UByte = (mask1 | mask2).toByte
        //e.g. current bit = 3 --> mask =    1110 0000
        //nbits = 3 --> totalBits = 6
        //                                                 mask=     1110 0000
        //                                                 and         0000 0011 <- masks[totalBits - 1]
        //	                                                            -----------
        //					final mask         1110 0011
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v << (8 - totalBits))).toByte
        pBitStrm.currentBit += nbits.toInt
        if pBitStrm.currentBit == 8 then
            pBitStrm.currentBit = 0
            pBitStrm.currentByte += 1
            bitstream_push_data_if_required(pBitStrm)

    } else {
        val totalBitsForNextByte: UByte = (totalBits - 8).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask1).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v >>> totalBitsForNextByte)).toByte
        pBitStrm.currentByte += 1
        bitstream_push_data_if_required(pBitStrm)
        val mask: UByte = (~masksb(8 - totalBitsForNextByte)).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v << (8 - totalBitsForNextByte))).toByte
        pBitStrm.currentBit = totalBitsForNextByte.toInt
    }

    assert(pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.buf.length * 8)
}

/* nbits 1..7*/
def BitStream_ReadPartialByte(pBitStrm: BitStream, nbits: UByte): Option[UByte] = {

    var v: UByte = 0
    val cb: UByte = pBitStrm.currentBit.toByte
    val totalBits: UByte = (cb + nbits).toByte

    if (totalBits <= 8) {
        v = ((pBitStrm.buf(pBitStrm.currentByte) >>> (8 - totalBits)) & masksb(nbits)).toByte
        pBitStrm.currentBit += nbits.toInt
        if pBitStrm.currentBit == 8 then
            pBitStrm.currentBit = 0
            pBitStrm.currentByte += 1
        bitstream_fetch_data_if_required(pBitStrm)

    } else {
        var totalBitsForNextByte: UByte = (totalBits - 8).toByte
        v = (pBitStrm.buf(pBitStrm.currentByte) << totalBitsForNextByte).toByte
        pBitStrm.currentByte += 1
        bitstream_fetch_data_if_required(pBitStrm)
        v = (v | pBitStrm.buf(pBitStrm.currentByte) >>> (8 - totalBitsForNextByte)).toByte
        v = (v & masksb(nbits)).toByte
        pBitStrm.currentBit = totalBitsForNextByte.toInt
    }

    if pBitStrm.currentByte * 8 + pBitStrm.currentBit <= pBitStrm.buf.length * 8 then
        Some(v)
    else
        None
}


/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/
/**     Integer Functions                                                                                                                                             **/
/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/
def BitStream_EncodeNonNegativeInteger32Neg(pBitStrm: BitStream, v: Int, negate: Boolean): Unit = {
    var cc: UInt = 0
    var curMask: UInt = 0
    var pbits: UInt = 0

    if v == 0 then
        return ()

    if v >>> 8 == 0 then
        cc = 8
        curMask = 0x80
    else if v >>> 16 == 0 then
        cc = 16
        curMask = 0x8000
    else if v >>> 24 == 0 then
        cc = 24
        curMask = 0x800000
    else
        cc = 32
        curMask = 0x80000000

    while (v & curMask) == 0 do
        decreases(cc)
        curMask >>>= 1
        cc -= 1

    pbits = cc % 8
    if pbits > 0 then
        cc -= pbits
        BitStream_AppendPartialByte(pBitStrm, (v >>> cc).toByte, pbits.toByte, negate)

    while cc > 0 do
        decreases(cc)
        val t1: UInt = v.toInt & masks2(cc >>> 3)
        cc -= 8
        BitStream_AppendByte(pBitStrm, (t1 >>> cc).toByte, negate)
}
def BitStream_DecodeNonNegativeInteger32Neg(pBitStrm: BitStream, nBitsVal: Int): Option[UInt] = {

    var v: UInt = 0

    var nBits = nBitsVal
    while nBits >= 8 do
        decreases(nBits)
        v = v << 8

        BitStream_ReadByte(pBitStrm) match
            case None => return None
            case Some(ub) =>
                // mask the Byte-Bits, becuase negative values eg. -1 (1111 1111)
                // will be casted to an Int -1 (1111 ... 1111)
                v = v | (ub & 0xFF)

        nBits -= 8

    if nBits != 0 then
        v = v << nBits
        BitStream_ReadPartialByte(pBitStrm, nBits.toByte) match
            case None => return None
            case Some(ub) => v = v | (ub & 0xFF)

    Some(v)
}
def BitStream_EncodeNonNegativeInteger(pBitStrm: BitStream, v: ULong): Unit = {
    // TODO: support WORD_SIZE=4?
    //if WORD_SIZE == 8 then
    if v >>> 32 == 0 then
        BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, v.toInt, false)
    else
        // TODO: Check Int/Long
        val hi = (v >>> 32).toInt
        val lo = v.toInt
        BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, hi, false)

        val nBits: Int = GetNumberOfBitsForNonNegativeInteger(lo.toLong << 32 >>> 32) // TODO: is this easier?
        BitStream_AppendNBitZero(pBitStrm, 32 - nBits)
        BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, lo, false)
    //else
    //    BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, v.toInt, false)
}
def BitStream_DecodeNonNegativeInteger(pBitStrm: BitStream, nBits: Int): Option[ULong] = {
    // TODO: support WORD_SIZE=4?
    // if WORD_SIZE == 8 then
    if nBits <= 32 then
        BitStream_DecodeNonNegativeInteger32Neg(pBitStrm, nBits) match
            case None => return None
            case Some(lo) =>
                return Some(lo & 0xFFFFFFFFL)

    val hi_ret = BitStream_DecodeNonNegativeInteger32Neg(pBitStrm, 32)
    val lo_ret = BitStream_DecodeNonNegativeInteger32Neg(pBitStrm, nBits - 32)

    (hi_ret, lo_ret) match
        case (Some(hi), Some(lo)) =>
            var v: ULong = hi & 0xFFFFFFFFL
            v = v << nBits - 32L
            v |= lo & 0xFFFFFFFFL
            return Some(v)
        case _ => return None
    //else
    //    return BitStream_DecodeNonNegativeInteger32Neg(pBitStrm, v, nBits)
}

def BitStream_EncodeNonNegativeIntegerNeg(pBitStrm: BitStream, v: ULong, negate: Boolean): Unit = {
    //if WORD_SIZE == 8 then
    if v >>> 32 == 0 then
        BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, v.toInt, negate)
    else
        // TODO: Check Int/Long
        val hi = (v >>> 32).toInt
        var lo = v.toInt
        BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, hi, negate)

        /*bug !!!!*/
        if negate then
            lo = ~lo
        val nBits = GetNumberOfBitsForNonNegativeInteger(lo.toLong)
        BitStream_AppendNBitZero(pBitStrm, 32 - nBits)
        BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, lo, false)
    //else
    //    BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, v, negate)

}

def GetNumberOfBitsForNonNegativeInteger32(vVal: Int): Int = {
    var ret: Int = 0

    var v = vVal
    if v >>> 8 == 0 then
        ret = 0
    else if v >>> 16 == 0 then
        ret = 8
        v = v >>> 8
    else if v >>> 24 == 0 then
        ret = 16
        v = v >>> 16
    else
        ret = 24
        v = v >>> 24

    while v > 0 do
        decreases(v)
        v = v >>> 1
        ret += 1

    return ret
}
def GetNumberOfBitsForNonNegativeInteger(v: ULong): Int = {
    if WORD_SIZE == 8 then
        if v < 0x100000000L then
            return GetNumberOfBitsForNonNegativeInteger32(v.toInt)
        else
            val hi = (v >>> 32).toInt
            return 32 + GetNumberOfBitsForNonNegativeInteger32(hi)
    else
        return GetNumberOfBitsForNonNegativeInteger32(v.toInt)
}

def GetLengthInBytesOfUInt (v: ULong): Int = {
    var ret: Int = 0
    var v32: UInt = v.toInt
    //if (WORD_SIZE == 8) {
    if v > 0xFFFFFFFF.toLong then
        ret = 4
        v32 = (v >>> 32).toInt
    // }

    if v32 < 0x100 then
        return ret + 1
    if v32 < 0x10000 then
        return ret + 2
    if v32 < 0x1000000 then
        return ret + 3

    return ret + 4
}

def GetLengthSIntHelper(v: ULong): Int = {
    var ret: Int = 0
    var v32: UInt = v.toInt
    //#if WORD_SIZE == 8
    if v > 0x7FFFFFFF then
        ret = 4
        v32 = (v >>> 32).toInt
    //#endif

    if v32 <= 0x7F then
        return ret + 1
    if v32 <= 0x7FFF then
        return ret + 2
    if v32 <= 0x7FFFFF then
        return ret + 3
    return ret + 4
}

def GetLengthInBytesOfSInt (v: Long): Int = {
    if v >= 0 then
        return GetLengthSIntHelper(v)

    return GetLengthSIntHelper((-v - 1))
}


def BitStream_EncodeConstraintWholeNumber(pBitStrm: BitStream, v: Long, min: Long, max: Long): Unit = {
    require(min <= max)
    val range = (max - min)
    if range == 0 then
        return

    val nRangeBits: Int = GetNumberOfBitsForNonNegativeInteger(range)
    val nBits: Int = GetNumberOfBitsForNonNegativeInteger((v - min))
    BitStream_AppendNBitZero(pBitStrm, nRangeBits - nBits);
    BitStream_EncodeNonNegativeInteger(pBitStrm, (v - min))
}

def BitStream_EncodeConstraintPosWholeNumber(pBitStrm: BitStream, v: ULong, min: ULong, max: ULong): Unit = {
    assert(min <= v)
    assert(v <= max)
    val range: ULong = (max - min)
    if range == 0 then
        return
    val nRangeBits: Int = GetNumberOfBitsForNonNegativeInteger(range)
    val nBits: Int = GetNumberOfBitsForNonNegativeInteger(v - min)
    BitStream_AppendNBitZero(pBitStrm, nRangeBits - nBits)
    BitStream_EncodeNonNegativeInteger(pBitStrm, v - min)
}


def BitStream_DecodeConstraintWholeNumber(pBitStrm: BitStream, min: Long, max: Long): Option[Long] = {

    val range: ULong = (max - min)

//    ASSERT_OR_RETURN_FALSE(min <= max);

    if range == 0 then
        return Some(min)

    val nRangeBits = GetNumberOfBitsForNonNegativeInteger(range)

    BitStream_DecodeNonNegativeInteger(pBitStrm, nRangeBits) match
        case None => return None
        case Some(ul) => return Some(ul + min)
}


def BitStream_DecodeConstraintWholeNumberByte(pBitStrm: BitStream, min: Byte, max: Byte): Option[Byte] = {

    BitStream_DecodeConstraintWholeNumber(pBitStrm, min.toLong, max.toLong) match
        case None => None
        case Some(l) => Some(l.toByte)
}


def BitStream_DecodeConstraintWholeNumberShort(pBitStrm: BitStream, min: Short, max: Short): Option[Short] = {

    BitStream_DecodeConstraintWholeNumber(pBitStrm, min.toLong, max.toLong) match
        case None => None
        case Some(l) => Some(l.toShort)
}


def BitStream_DecodeConstraintWholeNumberInt(pBitStrm: BitStream, min: Int, max: Int): Option[Int] = {

    BitStream_DecodeConstraintWholeNumber(pBitStrm, min.toLong, max.toLong) match
        case None => None
        case Some(l) => Some(l.toInt)
}


def BitStream_DecodeConstraintWholeNumberUByte(pBitStrm: BitStream, min: UByte, max: UByte): Option[UByte] = {

    BitStream_DecodeConstraintWholeNumber(pBitStrm, min.toLong, max.toLong) match
        case None => None
        case Some(l) => Some(l.toByte)
}


def BitStream_DecodeConstraintWholeNumberUShort(pBitStrm: BitStream, min: UShort, max: UShort): Option[UShort] = {

    BitStream_DecodeConstraintWholeNumber(pBitStrm, min.toLong, max.toLong) match
        case None => None
        case Some(l) => Some(l.toShort)
}


def BitStream_DecodeConstraintWholeNumberUInt(pBitStrm: BitStream, min: UInt, max: UInt): Option[UInt] = {

    BitStream_DecodeConstraintWholeNumber(pBitStrm, min.toLong, max.toLong) match
        case None => None
        case Some(l) => Some(l.toInt)
}


def BitStream_DecodeConstraintPosWholeNumber(pBitStrm: BitStream, min: ULong, max: ULong): Option[ULong] = {
    val range: ULong = max - min

    //ASSERT_OR_RETURN_FALSE(min <= max);

    if range == 0 then
        return Some(min)

    val nRangeBits: Int = GetNumberOfBitsForNonNegativeInteger(range)

    BitStream_DecodeNonNegativeInteger(pBitStrm, nRangeBits) match
        case None => None
        case Some(uv) => Some(uv + min)
}

def BitStream_EncodeSemiConstraintWholeNumber(pBitStrm: BitStream, v: Long, min: Long): Unit = {
    assert(v >= min)
    val nBytes: Int = GetLengthInBytesOfUInt((v - min))

    /* encode length */
    BitStream_EncodeConstraintWholeNumber(pBitStrm, nBytes.toLong, 0, 255)
    /*8 bits, first bit is always 0*/
    /* put required zeros*/
    BitStream_AppendNBitZero(pBitStrm, nBytes * 8 - GetNumberOfBitsForNonNegativeInteger((v - min)))
    /*Encode number */
    BitStream_EncodeNonNegativeInteger(pBitStrm, (v - min))
}

def BitStream_EncodeSemiConstraintPosWholeNumber(pBitStrm: BitStream, v: ULong, min: ULong): Unit = {
    assert(v >= min)
    val nBytes: Int = GetLengthInBytesOfUInt(v - min)

    /* encode length */
    BitStream_EncodeConstraintWholeNumber(pBitStrm, nBytes.toLong, 0, 255)
    /*8 bits, first bit is always 0*/
    /* put required zeros*/
    BitStream_AppendNBitZero(pBitStrm, nBytes * 8 - GetNumberOfBitsForNonNegativeInteger(v - min))
    /*Encode number */
    BitStream_EncodeNonNegativeInteger(pBitStrm, v - min)
}


def BitStream_DecodeSemiConstraintWholeNumber(pBitStrm:BitStream, min: Long): Option[Long] = {

    var nBytes: Long = 0
    var v: Long = 0

    BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 255) match
        case None => return None
        case Some(l) => nBytes = l

    var i: Long = 0
    while i < nBytes do
        decreases(nBytes - i)

        BitStream_ReadByte(pBitStrm) match
            case None => return None
            case Some(ub) => v = (v << 8) | ub.toLong

        i += 1

    v += min

    return Some(v)
}


def BitStream_DecodeSemiConstraintPosWholeNumber(pBitStrm:BitStream, min: ULong): Option[ULong] = {

    var nBytes: Long = 0
    var v: ULong = 0
    BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 255) match
        case None => return None
        case Some(l) => nBytes = l

    var i: Long = 0
    while i < nBytes do
        decreases(nBytes - i)

        BitStream_ReadByte(pBitStrm) match
            case None => return None
            case Some(ub) => v = (v << 8) | ub.toLong

        i += 1
    v += min
    return Some(v)
}

def BitStream_EncodeUnConstraintWholeNumber(pBitStrm: BitStream, v: Long): Unit = {
    val nBytes: Int = GetLengthInBytesOfSInt(v)

    /* encode length */
    BitStream_EncodeConstraintWholeNumber(pBitStrm, nBytes.toLong, 0, 255)
    /*8 bits, first bit is always 0*/

    if v >= 0 then
        BitStream_AppendNBitZero(pBitStrm, nBytes * 8 - GetNumberOfBitsForNonNegativeInteger(v))
        BitStream_EncodeNonNegativeInteger(pBitStrm, v)
    else
        BitStream_AppendNBitOne(pBitStrm, nBytes * 8 - GetNumberOfBitsForNonNegativeInteger((-v - 1)))
        BitStream_EncodeNonNegativeIntegerNeg(pBitStrm, (-v - 1), true)
}


def BitStream_DecodeUnConstraintWholeNumber(pBitStrm: BitStream): Option[Long] = {

    var nBytes: Long = 0

    BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 255) match
        case None => return None
        case Some(l) => nBytes = l

    val valIsNegative: Boolean = BitStream_PeekBit(pBitStrm)

    var v: Long = if valIsNegative then Long.MaxValue else 0

    var i: Long = 0
    while i < nBytes do
        decreases(nBytes - i)

        BitStream_ReadByte(pBitStrm) match
            case None => return None
            case Some(ub) => v = (v << 8) | ub.toLong

        i += 1

    return Some(v)
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
ab: F    (0..3)
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
//#define ExpoBitMask    0x7F800000U
//#define MantBitMask    0x007FFFFFU
//#define MantBitMask2 0xF0000000U
//#define MantisaExtraBit 0x00800000U
//#endif


def CalculateMantissaAndExponent(d: Double): (Int, ULong) = {
    val ll: ULong = java.lang.Double.doubleToLongBits(d)

    var exponent: Int = 0
    var mantissa: ULong = 0

    //#if FP_WORD_SIZE == 8
    exponent = (((ll & ExpoBitMask) >>> 52) - 1023 - 52).toInt
    mantissa = ll & MantBitMask
    mantissa = mantissa | MantisaExtraBit
    //#else
    //exponent.x = (int)(((ll & ExpoBitMask) >>> 23) - 127 - 23);
    //mantissa.x = ll & MantBitMask;
    //mantissa.x |= MantisaExtraBit;
    //#endif
    return (exponent, mantissa)
}

def GetDoubleByMantissaAndExp(mantissa: ULong, exponentVal: Int): Double = {
    return java.lang.Double.longBitsToDouble(((exponentVal + 1023L + 52L) << 52L) | (mantissa & MantBitMask))
}

def BitStream_EncodeReal(pBitStrm: BitStream, vVal: Double): Unit = {
    var header: UByte = -0x80

    var v = vVal
    if (v == 0.0) {
        BitStream_EncodeConstraintWholeNumber(pBitStrm, 0, 0, 0xFF)
        return
    }
    if (v == Double.PositiveInfinity) {
        BitStream_EncodeConstraintWholeNumber(pBitStrm, 1, 0, 0xFF)
        BitStream_EncodeConstraintWholeNumber(pBitStrm, 0x40, 0, 0xFF)
        return
    }

    if (v == Double.NegativeInfinity) {
        BitStream_EncodeConstraintWholeNumber(pBitStrm, 1, 0, 0xFF)
        BitStream_EncodeConstraintWholeNumber(pBitStrm, 0x41, 0, 0xFF)
        return
    }

    if (v < 0) {
        header = (header | 0x40).toByte
        v = -v
    }

    val (exponent, mantissa) = CalculateMantissaAndExponent(v)
    val nExpLen: Int = GetLengthInBytesOfSInt(exponent.toLong)
    val nManLen: Int = GetLengthInBytesOfUInt(mantissa)
    assert(nExpLen <= 3)
    if nExpLen == 2 then
        header = (header | 1).toByte
    else if nExpLen == 3 then
        header = (header | 2).toByte

    /* encode length */
    BitStream_EncodeConstraintWholeNumber(pBitStrm, 1 + nExpLen + nManLen.toLong, 0, 0xFF)

    /* encode header */
    BitStream_EncodeConstraintWholeNumber(pBitStrm, header.toLong & 0xFF, 0, 0xFF)

    /* encode exponent */
    if exponent >= 0 then
        BitStream_AppendNBitZero(pBitStrm, nExpLen * 8 - GetNumberOfBitsForNonNegativeInteger(exponent))
        BitStream_EncodeNonNegativeInteger(pBitStrm, exponent)
    else
        BitStream_AppendNBitOne(pBitStrm, nExpLen * 8 - GetNumberOfBitsForNonNegativeInteger(-exponent - 1))
        BitStream_EncodeNonNegativeIntegerNeg(pBitStrm, -exponent - 1, true)

    /* encode mantissa */
    BitStream_AppendNBitZero(pBitStrm, nManLen * 8 - GetNumberOfBitsForNonNegativeInteger(mantissa))
    BitStream_EncodeNonNegativeInteger(pBitStrm, mantissa)
}


def BitStream_DecodeReal(pBitStrm: BitStream): Option[Double] = {
    BitStream_ReadByte(pBitStrm) match
        case None => return None
        case Some(length) =>
            if length == 0 then
                return Some(0.0)

            BitStream_ReadByte(pBitStrm) match
                case None => return None
                case Some(header) =>
                    if header == 0x40 then
                        return Some(Double.PositiveInfinity)

                    if header == 0x41 then
                        return Some(Double.NegativeInfinity)

                    return DecodeRealAsBinaryEncoding(pBitStrm, length.toInt - 1, header)
}


def DecodeRealAsBinaryEncoding(pBitStrm: BitStream, lengthVal: Int, header: UByte): Option[Double] = {

        var length = lengthVal
        var sign: Int = 1
        /*int base=2;*/
        var factor: ULong = 1
        var expFactor: Int = 1
        var N: ULong = 0

        if (header & 0x40) > 0 then
            sign = -1
        if (header & 0x10) > 0 then
            /*base = 8;*/
            expFactor = 3
        else if (header & 0x20) > 0 then
            /*base = 16;*/
            expFactor = 4

        val F: Int = ((header & 0x0C) >>> 2).toInt
        factor <<= F

        val expLen: Int = ((header & 0x03) + 1).toInt

        if expLen > length then
            return None

        val expIsNegative = BitStream_PeekBit(pBitStrm)
        var exponent: Int = if expIsNegative then 0xFFFFFFFF else 0

        var i: Int = 0
        while i < expLen do
            decreases(expLen - i)

            BitStream_ReadByte(pBitStrm) match
                case None => return None
                case Some(ub) => exponent = exponent << 8 | (ub.toInt & 0xFF)

            i += 1

        length -= expLen

        var j: Int = 0
        while j < length do
            decreases(length - j)

            BitStream_ReadByte(pBitStrm) match
                case None => return None
                case Some(ub) => N = N << 8 | (ub.toInt & 0xFF)

            j += 1

        /*    *v = N*factor * pow(base,exp);*/
        var v: Double = GetDoubleByMantissaAndExp(N * factor, expFactor * exponent)

        if sign < 0 then
            v = -v

        return Some(v)
}

def BitStream_checkBitPatternPresent(pBitStrm: BitStream, bit_terminated_pattern: Array[UByte], bit_terminated_pattern_size_in_bitsVal: UByte): Int = {
    var bit_terminated_pattern_size_in_bits = bit_terminated_pattern_size_in_bitsVal
    val tmp_currentByte: Int = pBitStrm.currentByte
    val tmp_currentBit: Int = pBitStrm.currentBit
    var tmp_byte: UByte = 0

    if pBitStrm.currentByte * 8 + pBitStrm.currentBit + bit_terminated_pattern_size_in_bits.toInt > pBitStrm.buf.length * 8 then
        return 0

    var i: Int = 0
    while bit_terminated_pattern_size_in_bits >= 8 do
        decreases(bit_terminated_pattern_size_in_bits)

        BitStream_ReadByte(pBitStrm) match
            case None => return 0
            case Some(ub) => tmp_byte = ub

        bit_terminated_pattern_size_in_bits = 8
        if bit_terminated_pattern(i) != tmp_byte then
            pBitStrm.currentByte = tmp_currentByte
            pBitStrm.currentBit = tmp_currentBit
            return 1
        i += 1

    if bit_terminated_pattern_size_in_bits > 0 then
        BitStream_ReadPartialByte(pBitStrm, bit_terminated_pattern_size_in_bits) match
            case None => return 0
            case Some(ub) => tmp_byte = ub

        tmp_byte = (tmp_byte << (8 - bit_terminated_pattern_size_in_bits)).toByte

        if bit_terminated_pattern(i) != tmp_byte then
            pBitStrm.currentByte = tmp_currentByte
            pBitStrm.currentBit = tmp_currentBit
            return 1

    return 2
}


def BitStream_ReadBits_nullterminated(pBitStrm: BitStream, bit_terminated_pattern: Array[UByte], bit_terminated_pattern_size_in_bits: UByte, nMaxReadBits: Int): Option[(Array[UByte], Int)] = {
    var checkBitPatternPresentResult: Int = 0

    var bitsRead: Int = 0

    val tmpStrm: BitStream = BitStream_Init(if nMaxReadBits % 8 == 0 then nMaxReadBits / 8 else nMaxReadBits / 8 + 1)

    checkBitPatternPresentResult = BitStream_checkBitPatternPresent(pBitStrm, bit_terminated_pattern, bit_terminated_pattern_size_in_bits)
    while (bitsRead < nMaxReadBits) && (checkBitPatternPresentResult == 1) do
        decreases(nMaxReadBits - bitsRead)
        BitStream_ReadBit(pBitStrm) match
            case None => return None
            case Some(bitVal) =>
                BitStream_AppendBit(tmpStrm, bitVal)
                bitsRead += 1

        if bitsRead < nMaxReadBits then
            checkBitPatternPresentResult = BitStream_checkBitPatternPresent(pBitStrm, bit_terminated_pattern, bit_terminated_pattern_size_in_bits)

    if (bitsRead == nMaxReadBits) && (checkBitPatternPresentResult == 1) then
        checkBitPatternPresentResult = BitStream_checkBitPatternPresent(pBitStrm, bit_terminated_pattern, bit_terminated_pattern_size_in_bits)

    if checkBitPatternPresentResult != 2 then
        return None

    return Some((tmpStrm.buf, bitsRead))
}


def BitStream_EncodeOctetString_no_length(pBitStrm: BitStream, arr: Array[UByte], nCount: Int): Boolean = {
    val cb = pBitStrm.currentBit
    var ret: Boolean = false

    if cb == 0 then
        //#ifdef ASN1SCC_STREAMING
//       var remainingBytesToSend: Int = nCount
//       while remainingBytesToSend > 0 do
//           decreases(remainingBytesToSend)
//           val currentBatch =
//               if pBitStrm.currentByte + remainingBytesToSend <= pBitStrm.buf.length then
//                   remainingBytesToSend
//               else
//                   pBitStrm.buf.length - pBitStrm.currentByte
//
//           //memcpy(pBitStrm.buf(pBitStrm.currentByte), arr, currentBatch) // STAINLESS: Array.copy
//           pBitStrm.currentByte += currentBatch
//           bitstream_push_data_if_required(pBitStrm)
//           remainingBytesToSend -= currentBatch

        //else
        ret = pBitStrm.currentByte + nCount <= pBitStrm.buf.length
        if ret then
            //memcpy(pBitStrm.buf(pBitStrm.currentByte), arr, nCount)
            arr.copyToArray(pBitStrm.buf, pBitStrm.currentByte, nCount)
            pBitStrm.currentByte += nCount
        //#endif

    else
        ret = BitStream_AppendByteArray(pBitStrm, arr, nCount)
        /*var i1 = 0
        while i1 < nCount && ret do
            decreases(nCount - i1)
            ret = BitStream_AppendByte0(pBitStrm, arr(i1))
            i1 += 1
        */
    ret
}


def BitStream_DecodeOctetString_no_length(pBitStrm: BitStream, nCount: Int): Option[Array[UByte]] = {
    val cb: Int = pBitStrm.currentBit
    var arr: Array[UByte] = Array.fill(nCount+1)(0)

    if cb == 0 then
        //#ifdef ASN1SCC_STREAMING
        //        var remainingBytesToRead: Int = nCount
        //        while remainingBytesToRead > 0 do
        //            decreases(remainingBytesToRead)
        //            val currentBatch: Int =
        //                if pBitStrm.currentByte + remainingBytesToRead <= pBitStrm.buf.length then
        //                    remainingBytesToRead else
        //                    pBitStrm.buf.length - pBitStrm.currentByte
        //
        //            Array.copy(pBitStrm.buf, pBitStrm.currentByte, arr, 0, currentBatch) // TODO: 0?
        //            //memcpy(arr, pBitStrm.buf(pBitStrm.currentByte), currentBatch) // STAINLESS: howto? Array.copy
        //            pBitStrm.currentByte += currentBatch
        //            bitstream_fetch_data_if_required(pBitStrm)
        //            remainingBytesToRead -= currentBatch
        //
        //#else
        if pBitStrm.currentByte + nCount > pBitStrm.buf.length then
            return None

        //memcpy(arr, pBitStrm.buf(pBitStrm.currentByte), nCount)
        arr = pBitStrm.buf.slice(pBitStrm.currentByte, pBitStrm.currentByte+Math.max(1, nCount))
        pBitStrm.currentByte += nCount
    //#endif

    else
        BitStream_ReadByteArray(pBitStrm, nCount) match
            case None => return None
            case Some(a) => arr = a

    return Some(arr)
}


def BitStream_EncodeOctetString_fragmentation(pBitStrm: BitStream, arr: Array[UByte], nCount: Int): Boolean = {
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
            BitStream_EncodeConstraintWholeNumber(pBitStrm, nRemainingItemsVar1.toLong, 0, 0xFF)
        else
            BitStream_AppendBit(pBitStrm, true)
            BitStream_EncodeConstraintWholeNumber(pBitStrm, nRemainingItemsVar1.toLong, 0, 0x7FFF)


        var i1: Int = nCurOffset1
        while i1 < (nCurOffset1 + nRemainingItemsVar1) && ret do
            decreases(nCurOffset1 + nRemainingItemsVar1 - i1)
            ret = BitStream_AppendByte0(pBitStrm, arr(i1))
            i1 += 1

    return ret
}

def BitStream_DecodeOctetString_fragmentation(pBitStrm: BitStream, asn1SizeMax: Long): Option[Array[UByte]] = {
    var arr: Array[UByte] = Array.fill(asn1SizeMax.toInt)(0)
    var nCount: Int = 0

    var nLengthTmp1: Long = 0
    var nRemainingItemsVar1: Long = 0
    var nCurBlockSize1: Long = 0
    var nCurOffset1: Long = 0

    BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 0xFF) match
        case None => return None
        case Some(l) => nRemainingItemsVar1 = l

    while (nRemainingItemsVar1 & 0xC0) == 0xC0 do
        //decreases()
        if nRemainingItemsVar1 == 0xC4 then
            nCurBlockSize1 = 0x10000
        else if nRemainingItemsVar1 == 0xC3 then
            nCurBlockSize1 = 0xC000
        else if nRemainingItemsVar1 == 0xC2 then
            nCurBlockSize1 = 0x8000
        else if nRemainingItemsVar1 == 0xC1 then
            nCurBlockSize1 = 0x4000
        else
            return None

        var i1: Int = nCurOffset1.toInt
        while (nCurOffset1 + nCurBlockSize1 <= asn1SizeMax) && (i1 < (nCurOffset1 + nCurBlockSize1).toInt) do
            decreases((nCurOffset1 + nCurBlockSize1).toInt - i1)
            BitStream_ReadByte(pBitStrm) match
                case None => return None
                case Some(ub) => arr(i1) = ub
            i1 += 1

        nLengthTmp1 += nCurBlockSize1
        nCurOffset1 += nCurBlockSize1
        BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 0xFF) match
            case None => return None
            case Some(l) => nRemainingItemsVar1 = l

    if (nRemainingItemsVar1 & 0x80) > 0 then

        nRemainingItemsVar1 <<= 8
        BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 0xFF) match
            case None => return None
            case Some(l) =>
                nRemainingItemsVar1 |= l
                nRemainingItemsVar1 &= 0x7FFF

    if (nCurOffset1 + nRemainingItemsVar1 <= asn1SizeMax) then
        var i1: Int = nCurOffset1.toInt
        while i1 < (nCurOffset1 + nRemainingItemsVar1).toInt do
            decreases((nCurOffset1 + nRemainingItemsVar1).toInt - i1)
            BitStream_ReadByte(pBitStrm) match
                case None => return None
                case Some(ub) => arr(i1) = ub
            i1 += 1

        nLengthTmp1 += nRemainingItemsVar1

        if (nLengthTmp1 >= 1) && (nLengthTmp1 <= asn1SizeMax) then
            return Some(arr.take(nLengthTmp1.toInt))
        else
            return None

    return None
}


def BitStream_EncodeOctetString(pBitStrm: BitStream, arr: Array[UByte], nCount: Int, asn1SizeMin: Long, asn1SizeMax: Long): Boolean = {
    var ret: Boolean = nCount.toLong >= asn1SizeMin && nCount.toLong <= asn1SizeMax

    if ret then
        if asn1SizeMax < 65536 then
            if asn1SizeMin != asn1SizeMax then
                BitStream_EncodeConstraintWholeNumber(pBitStrm, nCount.toLong, asn1SizeMin, asn1SizeMax)
            ret = BitStream_EncodeOctetString_no_length(pBitStrm, arr, nCount)

        else
            ret = BitStream_EncodeOctetString_fragmentation(pBitStrm, arr, nCount)

    return ret
}


def BitStream_DecodeOctetString(pBitStrm: BitStream, asn1SizeMin: Long, asn1SizeMax: Long): Option[Array[UByte]] = {

    if asn1SizeMax < 65536 then
        var nCount: Int = 0
        if asn1SizeMin != asn1SizeMax then
            BitStream_DecodeConstraintWholeNumber(pBitStrm, asn1SizeMin, asn1SizeMax) match
                case None => return None
                case Some(l) => nCount = l.toInt
        else
            nCount = asn1SizeMin.toInt

        if (nCount >= asn1SizeMin && nCount <= asn1SizeMax) then
            return BitStream_DecodeOctetString_no_length(pBitStrm, nCount)
        else
            return None

    else
        return BitStream_DecodeOctetString_fragmentation(pBitStrm, asn1SizeMax)

}


def BitStream_EncodeBitString(pBitStrm: BitStream, arr: Array[UByte], nCount: Int, asn1SizeMin: Long, asn1SizeMax: Long): Boolean = {
    if asn1SizeMax < 65536 then
        if asn1SizeMin != asn1SizeMax then
            BitStream_EncodeConstraintWholeNumber(pBitStrm, nCount.toLong, asn1SizeMin, asn1SizeMax)

        BitStream_AppendBits(pBitStrm, arr, nCount)
    else
        var nRemainingItemsVar1: Long = nCount.toLong
        var nCurBlockSize1: Long = 0
        var nCurOffset1: Long = 0
        while nRemainingItemsVar1 >= 0x4000 do
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

            val t: Array[UByte] = Array.fill(nCurBlockSize1.toInt)(0)// STAINLESS: arr.slice((nCurOffset1 / 8).toInt, (nCurOffset1 / 8).toInt + nCurBlockSize1.toInt)
            BitStream_AppendBits(pBitStrm, t, nCurBlockSize1.toInt)
            nCurOffset1 += nCurBlockSize1
            nRemainingItemsVar1 -= nCurBlockSize1


        if nRemainingItemsVar1 <= 0x7F then
            BitStream_EncodeConstraintWholeNumber(pBitStrm, nRemainingItemsVar1, 0, 0xFF)
        else
            BitStream_AppendBit(pBitStrm, true)
            BitStream_EncodeConstraintWholeNumber(pBitStrm, nRemainingItemsVar1, 0, 0x7FFF)

        val t: Array[UByte] = Array.fill(nRemainingItemsVar1.toInt)(0) // STAINLESS: arr.slice((nCurOffset1 / 8).toInt, (nCurOffset1 / 8).toInt + nRemainingItemsVar1.toInt)
        BitStream_AppendBits(pBitStrm, t, nRemainingItemsVar1.toInt)

    return true
}


def BitStream_DecodeBitString(pBitStrm: BitStream, asn1SizeMin: Long, asn1SizeMax: Long): Option[Array[UByte]] = {

    if (asn1SizeMax < 65536) {
        var nCount: Long = 0
        if asn1SizeMin != asn1SizeMax then
            BitStream_DecodeConstraintWholeNumber(pBitStrm, asn1SizeMin, asn1SizeMax) match
                case None => return None
                case Some(l) => nCount = l
        else
            nCount = asn1SizeMin

        return BitStream_ReadBits(pBitStrm, nCount.toInt)

    } else {
        var nRemainingItemsVar1: Long = 0
        var nCurBlockSize1: Long = 0
        var nCurOffset1: Long = 0
        var nLengthTmp1: Long = 0
        BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 0xFF) match
            case None => return None
            case Some(l) => nRemainingItemsVar1 = l

        var arr: Array[UByte] = Array.fill(asn1SizeMax.toInt)(0)
        while (nRemainingItemsVar1 & 0xC0) == 0xC0 do
            //decreases()
            if nRemainingItemsVar1 == 0xC4 then
                nCurBlockSize1 = 0x10000
            else if nRemainingItemsVar1 == 0xC3 then
                nCurBlockSize1 = 0xC000
            else if nRemainingItemsVar1 == 0xC2 then
                nCurBlockSize1 = 0x8000
            else if nRemainingItemsVar1 == 0xC1 then
                nCurBlockSize1 = 0x4000
            else
                return None

            /*COVERAGE_IGNORE*/
            if nCurOffset1 + nCurBlockSize1 > asn1SizeMax then
                return None
            /*COVERAGE_IGNORE*/

            BitStream_ReadBits(pBitStrm, nCurBlockSize1.toInt) match
                case None => return None
                case Some(t) =>
                    Array.copy(t, 0, arr, (nCurOffset1 / 8).toInt, nCurBlockSize1.toInt)
                    nLengthTmp1 += nCurBlockSize1
                    nCurOffset1 += nCurBlockSize1
                    BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 0xFF) match
                        case None => return None
                        case Some(l) => nRemainingItemsVar1 = l

        if (nRemainingItemsVar1 & 0x80) > 0 then
            nRemainingItemsVar1 <<= 8
            BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 0xFF) match
                case None => return None
                case Some(l) =>
                    nRemainingItemsVar1 |= l
                    nRemainingItemsVar1 &= 0x7FFF

        if (nCurOffset1 + nRemainingItemsVar1 <= asn1SizeMax) then

            BitStream_ReadBits(pBitStrm, nRemainingItemsVar1.toInt) match
                case None => return None
                case Some(t) =>
                    Array.copy(t, 0, arr, (nCurOffset1 / 8).toInt, nRemainingItemsVar1.toInt)
                    nLengthTmp1 += nRemainingItemsVar1
                    if (nLengthTmp1 >= 1) && (nLengthTmp1 <= asn1SizeMax) then
                        return Some(arr)
    }
    return None
}

//#ifdef ASN1SCC_STREAMING
//def fetchData(pBitStrm: BitStream, fetchDataPrm: Option[Any]) = ???
//def pushData(pBitStrm: BitStream, pushDataPrm: Option[Any]) = ???
//
//
//def bitstream_fetch_data_if_required(pStrm: BitStream): Unit = {
//    if pStrm.currentByte == pStrm.buf.length && pStrm.fetchDataPrm != null then
//        fetchData(pStrm, pStrm.fetchDataPrm)
//        pStrm.currentByte = 0
//}
//def bitstream_push_data_if_required(pStrm: BitStream): Unit = {
//    if pStrm.currentByte == pStrm.buf.length && pStrm.pushDataPrm != null then
//        pushData(pStrm, pStrm.pushDataPrm)
//        pStrm.currentByte = 0
//}
//#endif

def bitstream_fetch_data_if_required(pStrm: BitStream): Unit = {}
def bitstream_push_data_if_required(pStrm: BitStream): Unit = {}
