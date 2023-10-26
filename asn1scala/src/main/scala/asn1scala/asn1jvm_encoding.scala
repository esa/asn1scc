package asn1scala

import stainless.*
import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}
import stainless.collection.*
import stainless.annotation.*
import stainless.proof.*
import stainless.math.*
import StaticChecks.*

val masks: Array[UByte] = Array(
    -0x80, // -128 / 1000 0000 / x80
     0x40, //   64 / 0100 0000 / x40
     0x20, //   32 / 0010 0000 / x20
     0x10, //   16 / 0001 0000 / x10
     0x08, //    8 / 0000 1000 / x08
     0x04, //    4 / 0000 0100 / x04
     0x02, //    2 / 0000 0010 / x02
     0x01, //    1 / 0000 0001 / x01
)

val masksb: Array[UByte] = Array(
     0x00, //   0 / 0000 0000 / x00
     0x01, //   1 / 0000 0001 / x01
     0x03, //   3 / 0000 0011 / x03
     0x07, //   7 / 0000 0111 / x07
     0x0F, //  15 / 0000 1111 / x0F
     0x1F, //  31 / 0001 1111 / x1F
     0x3F, //  63 / 0011 1111 / x3F
     0x7F, // 127 / 0111 1111 / x7F
    -0x1, //   -1 / 1111 1111 / xFF
)

val masks2: Array[UInt] = Array(
    0x00000000, //         0 / 0000 0000 0000 0000 0000 0000 0000 0000 / 0x0000 0000
    0x000000FF, //       255 / 0000 0000 0000 0000 0000 0000 1111 1111 / 0x0000 00FF
    0x0000FF00, //     65280 / 0000 0000 0000 0000 1111 1111 0000 0000 / 0x0000 FF00
    0x00FF0000, //  16711680 / 0000 0000 1111 1111 0000 0000 0000 0000 / 0x00FF 0000
    0xFF000000, // -16777216 / 1111 1111 0000 0000 0000 0000 0000 0000 / 0xFF00 0000
)


/***********************************************************************************************/
/**    Byte Stream Functions                                                                  **/
/***********************************************************************************************/
def ByteStream_Init(count: Int): ByteStream = {
    ByteStream(Array.fill(count)(0), 0, false)
}

@extern
def ByteStream_AttachBuffer(pStrm: ByteStream, buf: Array[UByte]): Unit = {
    pStrm.buf = buf // Illegal aliasing, therefore we need to workaround with this @extern...
    pStrm.currentByte = 0
}.ensuring(_ => pStrm.buf == buf && pStrm.currentByte == 0 && pStrm.EncodeWhiteSpace == old(pStrm).EncodeWhiteSpace)

def ByteStream_GetLength(pStrm: ByteStream): Int = {
    pStrm.currentByte
}

/***********************************************************************************************/
/**    Bit Stream Functions                                                                   **/
/***********************************************************************************************/
def BitString_equal(arr1: Array[UByte], arr2: Array[UByte]): Boolean = {
    arraySameElements(arr1, arr2)
    //return
    //    (nBitsLength1 == nBitsLength2) &&
    //        (nBitsLength1 / 8 == 0 || memcmp(arr1, arr2, nBitsLength1 / 8) == 0) &&
    //        (nBitsLength1 % 8 > 0 ? (arr1[nBitsLength1 / 8] >>> (8 - nBitsLength1 % 8) == arr2[nBitsLength1 / 8] >>> (8 - nBitsLength1 % 8)): TRUE);
}


def BitStream_Init(count: Int): BitStream = {
    BitStream(Array.fill(count)(0), 0, 0)
}

@extern
def BitStream_AttachBuffer(pBitStrm: BitStream, buf: Array[UByte]): Unit = {
    pBitStrm.buf = buf // Illegal aliasing, therefore we need to workaround with this @extern...
    pBitStrm.currentByte = 0
    pBitStrm.currentBit = 0
}.ensuring(_ => pBitStrm.buf == buf && pBitStrm.currentByte == 0 && pBitStrm.currentBit == 0)


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
or   00010000
-------------
     xxx1????
**/

def isPrefix(b1: BitStream, b2: BitStream): Boolean = {
    b1.buf.length <= b2.buf.length &&
      b1.bitIndex() <= b2.bitIndex() &&
      (b1.buf.length != 0) ==> arrayBitPrefix(b1.buf, b2.buf, 0, b1.bitIndex())
}

def isValidPair(w1: BitStream, w2: BitStream): Boolean = isPrefix(w1, w2)

@ghost
def reader(w1: BitStream, w2: BitStream): (BitStream, BitStream) = {
    require(isValidPair(w1, w2))
    val r1 = BitStream(snapshot(w2.buf), w1.currentByte, w1.currentBit)
    val r2 = BitStream(snapshot(w2.buf), w2.currentByte, w2.currentBit)
    (r1, r2)
}

@ghost @pure
def BitStream_ReadBitPure(pBitStrm: BitStream): (BitStream, Option[Boolean]) = {
    require(BitStream.validate_offset_bit(pBitStrm))
    val cpy = snapshot(pBitStrm)
    (cpy , BitStream_ReadBit(cpy))
}

@opaque @inlineOnce
def BitStream_AppendBitOne(pBitStrm: BitStream): Unit = {
    require(BitStream.validate_offset_bit(pBitStrm))
    @ghost val oldpBitStrm = snapshot(pBitStrm)

    val newB = (pBitStrm.buf(pBitStrm.currentByte) | masks(pBitStrm.currentBit)).toByte
    pBitStrm.buf(pBitStrm.currentByte) = newB

    ghostExpr {
       arrayUpdatedAtPrefixLemma(oldpBitStrm.buf, pBitStrm.currentByte, newB)
    }

    pBitStrm.increaseBitIndex()

}.ensuring { _ =>
    val w1 = old(pBitStrm)
    val w2 = pBitStrm
    w2.bitIndex() == w1.bitIndex() + 1
      &&& isValidPair(w1, w2)
      &&& {
        val (r1, r2) = reader(w1, w2)
        val (r2Got, bitGot) = BitStream_ReadBitPure(r1)
        bitGot.get == true && r2Got == r2
      }
      &&& BitStream.invariant(pBitStrm)
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
        --------
        xxx0????
**/
def BitStream_AppendBitZero(pBitStrm: BitStream): Unit = {
    require(BitStream.validate_offset_bits(pBitStrm, 1))
    val nmask = ~masks(pBitStrm.currentBit)
    pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & nmask).toByte

    pBitStrm.increaseBitIndex()
}.ensuring(_ => BitStream.invariant(pBitStrm))

def BitStream_AppendNBitZero(pBitStrm: BitStream, nbits: Int): Unit = {
    require(0 <= nbits)
    require(BitStream.validate_offset_bits(pBitStrm, nbits))

    val nBits = nbits % 8
    val nBytes = nbits / 8

    var new_currentBit: Int = pBitStrm.currentBit + nBits
    var new_currentByte: Int = pBitStrm.currentByte + nBytes

    if new_currentBit > 7 then
        new_currentBit = new_currentBit % 8
        new_currentByte += 1

    pBitStrm.currentBit = new_currentBit
    pBitStrm.currentByte = new_currentByte

}.ensuring(_ => BitStream.invariant(pBitStrm))

def BitStream_AppendNBitOne(pBitStrm: BitStream, nbitsVal: Int): Unit = {
    require(0 <= nbitsVal)
    require(BitStream.validate_offset_bits(pBitStrm, nbitsVal))
    var nbits = nbitsVal

    (while nbits > 0 do
        decreases(nbits)
        BitStream_AppendBitOne(pBitStrm)
        nbits -= 1
    ).invariant(nbits >= 0 &&& BitStream.validate_offset_bits(pBitStrm, nbits))
    ()
}

def BitStream_AppendBits(pBitStrm: BitStream, srcBuffer: Array[UByte], nbits: Int): Unit = {
    require(0 <= nbits && nbits/8 < srcBuffer.length)
    require(BitStream.validate_offset_bits(pBitStrm, nbits))
    var lastByte: UByte = 0

    val bytesToEncode: Int = nbits / 8
    val remainingBits: UByte = (nbits % 8).toByte

    BitStream_EncodeOctetString_no_length(pBitStrm, srcBuffer, bytesToEncode)

    if remainingBits > 0 then
        lastByte = ((srcBuffer(bytesToEncode) & 0xFF) >>> (8 - remainingBits)).toByte
        BitStream_AppendPartialByte(pBitStrm, lastByte, remainingBits, false)
}

def BitStream_AppendBit(pBitStrm: BitStream, v: Boolean): Unit = {
    require(BitStream.validate_offset_bits(pBitStrm, 1))
    if v then
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | masks(pBitStrm.currentBit)).toByte
    else
        val nmask = ~masks(pBitStrm.currentBit)
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & nmask).toByte

    pBitStrm.increaseBitIndex()
}.ensuring(_ => BitStream.invariant(pBitStrm))

// TODO check if needs Marios implementation
def BitStream_ReadBit(pBitStrm: BitStream): Option[Boolean] = {
    require(BitStream.validate_offset_bit(pBitStrm))
    val ret = (pBitStrm.buf(pBitStrm.currentByte) & masks(pBitStrm.currentBit)) != 0

    pBitStrm.increaseBitIndex()

    if pBitStrm.currentByte.toLong*8 + pBitStrm.currentBit <= pBitStrm.buf.length.toLong*8 then
        Some(ret)
    else
        None()
}.ensuring(_ => BitStream.invariant(pBitStrm))

def BitStream_PeekBit(pBitStrm: BitStream): Boolean = {
    require(pBitStrm.currentByte < pBitStrm.buf.length)
    ((pBitStrm.buf(pBitStrm.currentByte) & 0xFF) & (masks(pBitStrm.currentBit) & 0xFF)) > 0
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
and   11100000 (mask)
--------------
      xxx00000
or    000bbbbb
--------------
      xxxbbbbb

**/

@opaque @inlineOnce
def BitStream_AppendByte(pBitStrm: BitStream, value: Byte, negate: Boolean): Unit = {
    require(pBitStrm.bitIndex() + 8 <= pBitStrm.buf.length.toLong * 8)
    @ghost val oldpBitStrm = snapshot(pBitStrm)
    val cb = pBitStrm.currentBit.toByte
    val ncb = (8 - cb).toByte
    var mask = (~masksb(ncb)).toByte

    var v = value
    if negate then
        v = (~v).toByte

    pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask).toByte        // set bits right of currentbit to zero (where our value will be inserted)
    pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v >>>> cb)).toByte // set value into bits right of currentbit, but keep bits to the left
    pBitStrm.currentByte += 1

    ghostExpr {
        check(
            (oldpBitStrm.currentByte < oldpBitStrm.buf.length) ==>
                bytePrefix(
                    oldpBitStrm.buf(oldpBitStrm.currentByte),
                    pBitStrm.buf(oldpBitStrm.currentByte),
                    0, oldpBitStrm.currentBit))
    }
    @ghost val old2pBitStrm = snapshot(pBitStrm)

    if cb > 0 then
        mask = (~mask).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask).toByte         // set bits to the left of currentbit in next byte to zero (where the rest of our value will be inserted)
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v <<<< ncb)).toByte // set value into the bits left of currentbit, but keep the bits to the right

    ghostExpr {
        arrayUpdatedAtPrefixLemma(oldpBitStrm.buf, pBitStrm.currentByte - 1, pBitStrm.buf(pBitStrm.currentByte - 1))
        assert(arrayPrefix(oldpBitStrm.buf, old2pBitStrm.buf, 0, pBitStrm.currentByte - 1))

        if (cb > 0) {
            arrayUpdatedAtPrefixLemma(oldpBitStrm.buf, pBitStrm.currentByte, pBitStrm.buf(pBitStrm.currentByte))
            arrayUpdatedAtPrefixLemma(old2pBitStrm.buf, pBitStrm.currentByte, pBitStrm.buf(pBitStrm.currentByte))
            arrayPrefixTransitive(
                oldpBitStrm.buf,
                old2pBitStrm.buf,
                pBitStrm.buf,
                0, pBitStrm.currentByte - 1, pBitStrm.currentByte
            )
            check(arrayPrefix(
                oldpBitStrm.buf,
                pBitStrm.buf,
                0,
                oldpBitStrm.currentByte
            ))
        } else {
            check(arrayPrefix(
                oldpBitStrm.buf,
                pBitStrm.buf,
                0,
                oldpBitStrm.currentByte
            ))
        }
    }
}.ensuring { _ =>
    val w1 = old(pBitStrm)
    val w2 = pBitStrm
    w2.bitIndex() == w1.bitIndex() + 8 && isValidPair(w1, w2) && {
        val (r1, r2) = reader(w1, w2)
        val (r2Got, vGot) = BitStream_ReadBytePure(r1)
        vGot.get == value && r2Got == r2
    } && BitStream.invariant(pBitStrm)
}

def BitStream_AppendByte0(pBitStrm: BitStream, v: UByte): Boolean = {
    require(pBitStrm.bitIndex() + 8 <= pBitStrm.buf.length.toLong * 8)
    val cb: UByte = pBitStrm.currentBit.toByte
    val ncb: UByte = (8-cb).toByte

    var mask = ~masksb(ncb)

    pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask).toByte
    pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v >>>> cb)).toByte
    pBitStrm.currentByte += 1

    if cb > 0 then
        if pBitStrm.currentByte >= pBitStrm.buf.length then
            return false
        mask = ~mask
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v <<<< ncb)).toByte

    true
}

def BitStream_AppendByteArray(pBitStrm: BitStream, arr: Array[UByte], arr_len: Int): Boolean = {
    require(0 <= arr_len && arr_len <= arr.length)
    require(pBitStrm.currentByte < pBitStrm.buf.length - arr_len)
    //static byte    masks[] = { 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 };
    //static byte masksb[] = { 0x00, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F, 0xFF };

    val cb: UByte = pBitStrm.currentBit.toByte
    val ncb: UByte = (8 - cb).toByte

    val mask: UByte = (~masksb(ncb)).toByte
    val nmask: UByte = (~mask).toByte

    //if (pBitStrm->currentByte + (int)arr_len + (cb > 0 ? 1 : 0) >= pBitStrm->count)
    if (pBitStrm.currentByte.toLong+arr_len)*8 + pBitStrm.currentBit > pBitStrm.buf.length.toLong*8 then
        return false

    if arr_len > 0 then
        val v: UByte = arr(0)
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask).toByte     //make zero right bits (i.e. the ones that will get the new value)
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | ((v & 0xFF) >>> cb)).toByte    //shift right and then populate current byte
        pBitStrm.currentByte += 1

        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & nmask).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | ((v & 0xFF) << ncb)).toByte

    var i: Int = 1
    (while i < arr_len-1 do
        decreases(arr_len-1-i)
        val v: UByte = arr(i)
        val v1: UByte = ((v & 0xFF) >>> cb).toByte
        val v2: UByte = ((v & 0xFF) << ncb).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | v1).toByte //shift right and then populate current byte
        pBitStrm.currentByte += 1
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | v2).toByte
        i += 1
    ).invariant(1 <= i &&& i <= arr_len-1 &&& pBitStrm.currentByte < pBitStrm.buf.length)

    if arr_len - 1 > 0 then
        val v: UByte = arr(arr_len - 1)
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask ).toByte            //make zero right bits (i.e. the ones that will get the new value)
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | ((v & 0xFF) >>> cb)).toByte    //shift right and then populate current byte
        pBitStrm.currentByte += 1

        if cb > 0 then
            pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & nmask).toByte
            pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | ((v & 0xFF) << ncb)).toByte

    true
}

def BitStream_ReadByte(pBitStrm: BitStream): Option[UByte] = {

    val cb: UByte = pBitStrm.currentBit.toByte
    val ncb: UByte = (8 - cb).toByte

    var v: UByte = (pBitStrm.buf(pBitStrm.currentByte) << cb).toByte
    pBitStrm.currentByte += 1

    if cb > 0 then
        v = (v | (pBitStrm.buf(pBitStrm.currentByte) & 0xFF) >>> ncb).toByte // TODO: check if & 0xFF is needed

    if pBitStrm.currentByte.toLong*8 + pBitStrm.currentBit <= pBitStrm.buf.length.toLong*8 then
        Some(v)
    else
        None()
}

@ghost @pure
def BitStream_ReadBytePure(pBitStrm: BitStream): (BitStream, Option[Byte]) = {
    require(pBitStrm.bitIndex() + 8 <= pBitStrm.buf.length.toLong * 8)
    val cpy = snapshot(pBitStrm)
    (cpy, BitStream_ReadByte(cpy))
}

def BitStream_ReadByteArray(pBitStrm: BitStream, arr_len: Int): OptionMut[Array[UByte]] = {
    val arr: Array[UByte] = Array.fill(arr_len)(0)

    val cb: UByte = pBitStrm.currentBit.toByte
    val ncb: UByte = (8 - cb).toByte

    if (pBitStrm.currentByte+arr_len).toLong*8 + cb.toInt > pBitStrm.buf.length.toLong*8 then
        return NoneMut()

    var i: Int = 0
    while i < arr_len do
        decreases(arr_len - i)
        arr(i) = (pBitStrm.buf(pBitStrm.currentByte) << cb).toByte
        pBitStrm.currentByte += 1
        arr(i) = (arr(i) | (pBitStrm.buf(pBitStrm.currentByte) & 0xFF) >>> ncb).toByte
        i += 1

    SomeMut(arr)
}

def BitStream_ReadBits(pBitStrm: BitStream, nbits: Int): OptionMut[Array[UByte]] = {
    val bytesToRead: Int = nbits / 8
    val remainingBits: UByte = (nbits % 8).toByte

    BitStream_DecodeOctetString_no_length(pBitStrm, bytesToRead) match
        case NoneMut() => return NoneMut()
        case SomeMut(arr) =>
            if remainingBits > 0 then
                BitStream_ReadPartialByte(pBitStrm, remainingBits) match
                    case None() => return NoneMut()
                    case Some(ub) => arr(bytesToRead) = ub
                arr(bytesToRead) = (arr(bytesToRead) << (8 - remainingBits)).toByte
                SomeMut(arr)
            else
                SomeMut(arr)
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

    } else {
        val totalBitsForNextByte: UByte = (totalBits - 8).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask1).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v >>> totalBitsForNextByte)).toByte
        pBitStrm.currentByte += 1
        val mask: UByte = (~masksb(8 - totalBitsForNextByte)).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) & mask).toByte
        pBitStrm.buf(pBitStrm.currentByte) = (pBitStrm.buf(pBitStrm.currentByte) | (v << (8 - totalBitsForNextByte))).toByte
        pBitStrm.currentBit = totalBitsForNextByte.toInt
    }

    assert(pBitStrm.currentByte.toLong*8 + pBitStrm.currentBit <= pBitStrm.buf.length.toLong*8)
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

    } else {
        var totalBitsForNextByte: UByte = (totalBits - 8).toByte
        v = (pBitStrm.buf(pBitStrm.currentByte) << totalBitsForNextByte).toByte
        pBitStrm.currentByte += 1
        v = (v | (pBitStrm.buf(pBitStrm.currentByte) & 0xFF) >>> (8 - totalBitsForNextByte)).toByte
        v = (v & masksb(nbits)).toByte
        pBitStrm.currentBit = totalBitsForNextByte.toInt
    }

    if pBitStrm.currentByte.toLong*8 + pBitStrm.currentBit <= pBitStrm.buf.length.toLong*8 then
        Some(v)
    else
        None()
}


/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/
/**     Integer Functions                                                                     **/
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
            case None() => return None()
            case Some(ub) =>
                // mask the Byte-Bits, becuase negative values eg. -1 (1111 1111)
                // will be casted to an Int -1 (1111 ... 1111)
                v = v | (ub & 0xFF)

        nBits -= 8

    if nBits != 0 then
        v = v << nBits
        BitStream_ReadPartialByte(pBitStrm, nBits.toByte) match
            case None() => return None()
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
            case None() => return None()
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
        case _ => return None()
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

def BitStream_EncodeConstraintWholeNumber(pBitStrm: BitStream, v: Long, min: Long, max: Long): Unit = {
    require(min <= max)
    require(min <= v && v <= max)

    val range = max - min
    if range == 0 then
        return

    val nRangeBits: Int = GetNumberOfBitsForNonNegativeInteger(range)
    val nBits: Int = GetNumberOfBitsForNonNegativeInteger((v - min))
    BitStream_AppendNBitZero(pBitStrm, nRangeBits - nBits);
    BitStream_EncodeNonNegativeInteger(pBitStrm, (v - min))
}


def BitStream_EncodeConstraintPosWholeNumber(pBitStrm: BitStream, v: ULong, min: ULong, max: ULong): Unit = {
    require(max >= 0 && max <= Long.MaxValue)
    require(min >= 0 && min <= max)
    require(min <= v && v <= max)

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
        case None() => return None()
        case Some(ul) => return Some(ul + min)
}


def BitStream_DecodeConstraintWholeNumberByte(pBitStrm: BitStream, min: Byte, max: Byte): Option[Byte] = {

    BitStream_DecodeConstraintWholeNumber(pBitStrm, min.toLong, max.toLong) match
        case None() => None()
        case Some(l) => Some(l.toByte)
}


def BitStream_DecodeConstraintWholeNumberShort(pBitStrm: BitStream, min: Short, max: Short): Option[Short] = {

    BitStream_DecodeConstraintWholeNumber(pBitStrm, min, max) match
        case None() => None()
        case Some(l) => Some(l.toShort)
}


def BitStream_DecodeConstraintWholeNumberInt(pBitStrm: BitStream, min: Int, max: Int): Option[Int] = {

    BitStream_DecodeConstraintWholeNumber(pBitStrm, min, max) match
        case None() => None()
        case Some(l) => Some(l.toInt)
}


def BitStream_DecodeConstraintWholeNumberUByte(pBitStrm: BitStream, min: UByte, max: UByte): Option[UByte] = {

    BitStream_DecodeConstraintWholeNumber(pBitStrm, min.unsignedToLong, max.unsignedToLong) match
        case None() => None()
        case Some(l) => Some(l.toByte)
}

def BitStream_DecodeConstraintWholeNumberUShort(pBitStrm: BitStream, min: UShort, max: UShort): Option[UShort] = {

    BitStream_DecodeConstraintWholeNumber(pBitStrm, min.unsignedToLong, max.unsignedToLong) match
        case None() => None()
        case Some(l) => Some(l.toShort)
}

def BitStream_DecodeConstraintWholeNumberUInt(pBitStrm: BitStream, min: UInt, max: UInt): Option[UInt] = {

    BitStream_DecodeConstraintWholeNumber(pBitStrm, min.unsignedToLong, max.unsignedToLong) match
        case None() => None()
        case Some(l) => Some(l.toInt)
}

def BitStream_DecodeConstraintPosWholeNumber(pBitStrm: BitStream, min: ULong, max: ULong): Option[ULong] = {
    require(max >= 0 && max <= Long.MaxValue)
    require(min >= 0 && min <= max)

    val range: ULong = max - min

    if range == 0 then
        return Some(min)

    val nRangeBits: Int = GetNumberOfBitsForNonNegativeInteger(range)

    BitStream_DecodeNonNegativeInteger(pBitStrm, nRangeBits) match
        case None() => None()
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
        case None() => return None()
        case Some(l) => nBytes = l

    var i: Long = 0
    while i < nBytes do
        decreases(nBytes - i)

        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => v = (v << 8) | (ub & 0xFF).toLong

        i += 1

    v += min

    return Some(v)
}


def BitStream_DecodeSemiConstraintPosWholeNumber(pBitStrm:BitStream, min: ULong): Option[ULong] = {

    var nBytes: Long = 0
    var v: ULong = 0
    BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 255) match
        case None() => return None()
        case Some(l) => nBytes = l

    var i: Long = 0
    while i < nBytes do
        decreases(nBytes - i)

        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => v = (v << 8) | (ub & 0xFF).toLong

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
        case None() => return None()
        case Some(l) => nBytes = l

    val valIsNegative: Boolean = BitStream_PeekBit(pBitStrm)

    var v: Long = if valIsNegative then Long.MaxValue else 0

    var i: Long = 0
    while i < nBytes do
        decreases(nBytes - i)

        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => v = (v << 8) | (ub & 0xFF).toLong

        i += 1

    return Some(v)
}

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

def CalculateMantissaAndExponent(dAsll: Long): (UInt, ULong) = {
    require({
        val rawExp = (dAsll & ExpoBitMask) >>> DoubleNoOfMantissaBits
        rawExp >= 0 &&& rawExp <= ((1 << 11) - 2) // 2046, 2047 is the infinity case - never end up here with infinity
    })

    val exponent: UInt = ((dAsll & ExpoBitMask) >>> DoubleNoOfMantissaBits).toInt - DoubleBias.toInt - DoubleNoOfMantissaBits.toInt
    var mantissa: ULong = dAsll & MantissaBitMask
    mantissa = mantissa | MantissaExtraBit

    (exponent, mantissa)

}.ensuring((e, m) => e >= (-DoubleBias - DoubleNoOfMantissaBits) &&& e <= (DoubleBias - DoubleNoOfMantissaBits)
    &&& m >= 0 &&& m <= MantissaBitMask)

/**
Helper function for REAL encoding

Negative Ints always need 4 bytes of space, the ASN.1 standard compacts those numbers down
to 8, 16 or 24 bits depending on the leading bytes full of 1s.

Example:
-4 in Int: 0b1111_..._1111_1100
--> compacted to 0b1111_1100

The ASN.1 header holds the detail on how to interprete this number
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

def GetDoubleBitStringByMantissaAndExp(mantissa: ULong, exponentVal: Int): Long = {
    ((exponentVal + DoubleBias + DoubleNoOfMantissaBits) << DoubleNoOfMantissaBits) | (mantissa & MantissaBitMask)
}

@extern
def BitStream_EncodeReal(pBitStrm: BitStream, vVal: Double): Unit = {
    BitStream_EncodeRealBitString(pBitStrm, java.lang.Double.doubleToRawLongBits(vVal))
}

def BitStream_EncodeRealBitString(pBitStrm: BitStream, vVal: Long): Unit = {
    // according to X.690 2002

    var v = vVal

    // 8.5.2
    if ((v & InverseSignBitMask) == DoubleZeroBitString) {
        BitStream_EncodeConstraintWholeNumber(pBitStrm, 0, 0, 0xFF)
        return
    }

    // 8.5.9 SpecialRealValues (2021 standard)
    if(v & ExpoBitMask) == ExpoBitMask then

        // 8.5.9 PLUS-INFINITY
        if v == DoublePosInfBitString then
            BitStream_EncodeConstraintWholeNumber(pBitStrm, 1, 0, 0xFF)
            BitStream_EncodeConstraintWholeNumber(pBitStrm, 0x40, 0, 0xFF)
            return

        // 8.5.9 MINUS-INFINITY
        else if v == DoubleNegInfBitString then
            BitStream_EncodeConstraintWholeNumber(pBitStrm, 1, 0, 0xFF)
            BitStream_EncodeConstraintWholeNumber(pBitStrm, 0x41, 0, 0xFF)
            return

        // 8.5.9 NOT-A-NUMBER
        else
            BitStream_EncodeConstraintWholeNumber(pBitStrm, 1, 0, 0xFF)
            BitStream_EncodeConstraintWholeNumber(pBitStrm, 0x42, 0, 0xFF)
            return

    // 8.5.5 a)
    // fixed encoding style to binary
    // 8.5.6.2 exp has always base 2 - bit 0x20 and 0x10 are always 0
    // 8.5.6.3 F value is always zero - bit 0x08 and 0x04 are always 0
    var header = 0x80

    // 8.5.6.1
    if ((v & SignBitMask) == SignBitMask) { // check sign bit
        header |= 0x40
        v &= InverseSignBitMask // clear sign bit
    }

    val (exponent, mantissa) = CalculateMantissaAndExponent(v)

    val nManLen: Int = GetLengthInBytesOfUInt(mantissa)
    assert(nManLen <= 7) // 52 bit

    val compactExp = RemoveLeadingFFBytesIfNegative(exponent)
    val nExpLen: Int = GetLengthInBytesOfUInt(compactExp)
    assert(nExpLen >= 1 && nExpLen <= 2)

    // 8.5.6.4
    if nExpLen == 2 then
        header |= 0x01
    else if nExpLen == 3 then // this will never happen with this implementation
        header |= 0x02

    /* encode length */
    BitStream_EncodeConstraintWholeNumber(pBitStrm, 1 + nExpLen + nManLen, 0, 0xFF)

    /* encode header */
    BitStream_EncodeConstraintWholeNumber(pBitStrm, header & 0xFF, 0, 0xFF)

    /* encode exponent */
    if exponent >= 0 then
        // fill with zeros to have a whole byte
        BitStream_AppendNBitZero(pBitStrm, nExpLen * 8 - GetNumberOfBitsForNonNegativeInteger(exponent))
        BitStream_EncodeNonNegativeInteger(pBitStrm, exponent)
    else
        BitStream_EncodeNonNegativeInteger(pBitStrm, compactExp)

    /* encode mantissa */
    BitStream_AppendNBitZero(pBitStrm, nManLen * 8 - GetNumberOfBitsForNonNegativeInteger(mantissa))
    BitStream_EncodeNonNegativeInteger(pBitStrm, mantissa)
}

@extern
def BitStream_DecodeReal(pBitStrm: BitStream): Option[Double] = {
    BitStream_DecodeRealBitString(pBitStrm) match
        case None() =>
            None()
        case Some(ll) =>
            Some(java.lang.Double.longBitsToDouble(ll))
}

def BitStream_DecodeRealBitString(pBitStrm: BitStream): Option[Long] = {
    BitStream_ReadByte(pBitStrm) match
        case None() => None()
        case Some(length) =>
            if length == 0 then
                return Some(0)

            BitStream_ReadByte(pBitStrm) match
                case None() => None()
                case Some(header) =>
                    if header == 0x40 then
                        return Some(DoublePosInfBitString)

                    if header == 0x41 then
                        return Some(DoubleNegInfBitString)

                    DecodeRealAsBinaryEncoding(pBitStrm, length.toInt - 1, header)
}


def DecodeRealAsBinaryEncoding(pBitStrm: BitStream, lengthVal: Int, header: UByte): Option[Long] = {
    require(lengthVal >= 0 && lengthVal <= Int.MaxValue)

    // 8.5.5 a)
    assert((header & 0x80) == 0x80)

    // 8.5.6.4
    val expLen = (header & 0x03) + 1
    // sanity check
    if expLen > lengthVal then
        return None()


    val expIsNegative = BitStream_PeekBit(pBitStrm)
    var exponent: Int = if expIsNegative then 0xFF_FF_FF_FF else 0

    var i: Int = 0
    (while i < expLen do
        decreases(expLen - i)

        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => exponent = exponent << 8 | (ub.toInt & 0xFF)

        i += 1
      ).invariant(i >= 0 && i <= expLen)

    val length = lengthVal - expLen
    var N: ULong = 0
    var j: Int = 0
    (while j < length do
        decreases(length - j)

        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => N = (N << 8) | (ub.toInt & 0xFF)

        j += 1
      ).invariant(j >= 0 && j <= length)

    /*    *v = N*factor * pow(base,exp);*/
    var factor = 1 << ((header & 0x0C) >>> 2)

    // parse base factor
    val expFactor: Int = header match
        case x if (x & 0x10) > 0 => 3 // 2^3 = 8
        case x if (x & 0x20) > 0 => 4 // 2^4 = 16
        case _ => 1 // 2^1 = 2

    var v: Long = GetDoubleBitStringByMantissaAndExp(N * factor, expFactor * exponent)

    if (header & 0x40) > 0 then
        v |= SignBitMask

    Some(v)
}

def BitStream_checkBitPatternPresent(pBitStrm: BitStream, bit_terminated_pattern: Array[UByte], bit_terminated_pattern_size_in_bitsVal: UByte): Int = {
    var bit_terminated_pattern_size_in_bits = bit_terminated_pattern_size_in_bitsVal
    val tmp_currentByte: Int = pBitStrm.currentByte
    val tmp_currentBit: Int = pBitStrm.currentBit
    var tmp_byte: UByte = 0

    if pBitStrm.currentByte.toLong*8 + pBitStrm.currentBit + bit_terminated_pattern_size_in_bits.toInt > pBitStrm.buf.length.toLong*8 then
        return 0

    var i: Int = 0
    while bit_terminated_pattern_size_in_bits >= 8 do
        decreases(bit_terminated_pattern_size_in_bits)

        BitStream_ReadByte(pBitStrm) match
            case None() => return 0
            case Some(ub) => tmp_byte = ub

        bit_terminated_pattern_size_in_bits = 8
        if bit_terminated_pattern(i) != tmp_byte then
            pBitStrm.currentByte = tmp_currentByte
            pBitStrm.currentBit = tmp_currentBit
            return 1
        i += 1

    if bit_terminated_pattern_size_in_bits > 0 then
        BitStream_ReadPartialByte(pBitStrm, bit_terminated_pattern_size_in_bits) match
            case None() => return 0
            case Some(ub) => tmp_byte = ub

        tmp_byte = (tmp_byte << (8 - bit_terminated_pattern_size_in_bits)).toByte

        if bit_terminated_pattern(i) != tmp_byte then
            pBitStrm.currentByte = tmp_currentByte
            pBitStrm.currentBit = tmp_currentBit
            return 1

    return 2
}


def BitStream_ReadBits_nullterminated(pBitStrm: BitStream, bit_terminated_pattern: Array[UByte], bit_terminated_pattern_size_in_bits: UByte, nMaxReadBits: Int): OptionMut[(Array[UByte], Int)] = {
    var checkBitPatternPresentResult: Int = 0

    var bitsRead: Int = 0

    val tmpStrm: BitStream = BitStream_Init(if nMaxReadBits % 8 == 0 then nMaxReadBits / 8 else nMaxReadBits / 8 + 1)

    checkBitPatternPresentResult = BitStream_checkBitPatternPresent(pBitStrm, bit_terminated_pattern, bit_terminated_pattern_size_in_bits)
    while (bitsRead < nMaxReadBits) && (checkBitPatternPresentResult == 1) do
        decreases(nMaxReadBits - bitsRead)
        BitStream_ReadBit(pBitStrm) match
            case None() => return NoneMut()
            case Some(bitVal) =>
                BitStream_AppendBit(tmpStrm, bitVal)
                bitsRead += 1

        if bitsRead < nMaxReadBits then
            checkBitPatternPresentResult = BitStream_checkBitPatternPresent(pBitStrm, bit_terminated_pattern, bit_terminated_pattern_size_in_bits)

    if (bitsRead == nMaxReadBits) && (checkBitPatternPresentResult == 1) then
        checkBitPatternPresentResult = BitStream_checkBitPatternPresent(pBitStrm, bit_terminated_pattern, bit_terminated_pattern_size_in_bits)

    if checkBitPatternPresentResult != 2 then
        return NoneMut()

    return SomeMut((tmpStrm.buf, bitsRead))
}


def BitStream_EncodeOctetString_no_length(pBitStrm: BitStream, arr: Array[UByte], nCount: Int): Boolean = {
    val cb = pBitStrm.currentBit
    var ret: Boolean = false

    if cb == 0 then
        ret = pBitStrm.currentByte + nCount <= pBitStrm.buf.length
        if ret then
            copyToArray(arr, pBitStrm.buf, pBitStrm.currentByte, nCount)
            pBitStrm.currentByte += nCount

    else
        ret = BitStream_AppendByteArray(pBitStrm, arr, nCount)

    ret
}


def BitStream_DecodeOctetString_no_length(pBitStrm: BitStream, nCount: Int): OptionMut[Array[UByte]] = {
    val cb: Int = pBitStrm.currentBit
    val arr: Array[UByte] = Array.fill(nCount+1)(0)

    if cb == 0 then
        if pBitStrm.currentByte + nCount > pBitStrm.buf.length then
            return NoneMut()

        arrayCopyOffset(pBitStrm.buf, arr, pBitStrm.currentByte, pBitStrm.currentByte+nCount, 0)
        pBitStrm.currentByte += nCount

    else
        BitStream_ReadByteArray(pBitStrm, nCount) match
            case NoneMut() => return NoneMut()
            case SomeMut(a) => arrayCopyOffsetLen(a, arr, 0, 0, a.length)

    SomeMut(arr)
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

def BitStream_DecodeOctetString_fragmentation(pBitStrm: BitStream, asn1SizeMax: Long): OptionMut[Array[UByte]] = {
    require(asn1SizeMax >= 0 && asn1SizeMax < Int.MaxValue)

    val arr: Array[UByte] = Array.fill(asn1SizeMax.toInt)(0)
    var nCount: Int = 0

    var nLengthTmp1: Long = 0
    var nRemainingItemsVar1: Long = 0
    var nCurBlockSize1: Long = 0
    var nCurOffset1: Long = 0

    // get header data
    BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 0xFF) match
        case None() => return NoneMut()
        case Some(l) => nRemainingItemsVar1 = l

    // 11xx_xxxx header, there is a next fragment
    while (nRemainingItemsVar1 & 0xC0) == 0xC0 do
        decreases(asn1SizeMax - nCurOffset1) // TODO: check experimental decrease

        // get current block size
        if nRemainingItemsVar1 == 0xC4 then
            nCurBlockSize1 = 0x10000
        else if nRemainingItemsVar1 == 0xC3 then
            nCurBlockSize1 = 0xC000
        else if nRemainingItemsVar1 == 0xC2 then
            nCurBlockSize1 = 0x8000
        else if nRemainingItemsVar1 == 0xC1 then
            nCurBlockSize1 = 0x4000
        else
            return NoneMut()

        // fill current payload fragment into dest
        var i1: Int = nCurOffset1.toInt
        while (nCurOffset1 + nCurBlockSize1 <= asn1SizeMax) && (i1 < (nCurOffset1 + nCurBlockSize1).toInt) do
            decreases((nCurOffset1 + nCurBlockSize1).toInt - i1)
            BitStream_ReadByte(pBitStrm) match
                case None() => return NoneMut()
                case Some(ub) => arr(i1) = ub
            i1 += 1

        // sum combined length
        nLengthTmp1 += nCurBlockSize1
        // set offset for next run
        nCurOffset1 += nCurBlockSize1

        // get next header
        BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 0xFF) match
            case None() => return NoneMut()
            case Some(l) => nRemainingItemsVar1 = l

    // 1000_0000 header, last fragment has size bigger than 255 - current byte is upper, need to get lower
    if (nRemainingItemsVar1 & 0x80) > 0 then

        nRemainingItemsVar1 <<= 8 // put upper at correct position
        // get size (lower byte)
        BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 0xFF) match
            case None() => return NoneMut()
            case Some(l) =>
                nRemainingItemsVar1 |= l        // combine 15bit (7 upper, 8 lower) into size
                nRemainingItemsVar1 &= 0x7FFF   // clear the control bit

    if (nCurOffset1 + nRemainingItemsVar1 <= asn1SizeMax) then
        var i1: Int = nCurOffset1.toInt

        // fill last payload fragment into dest
        while i1 < (nCurOffset1 + nRemainingItemsVar1).toInt do
            decreases((nCurOffset1 + nRemainingItemsVar1).toInt - i1)
            BitStream_ReadByte(pBitStrm) match
                case None() => return NoneMut()
                case Some(ub) => arr(i1) = ub
            i1 += 1

        // add remainingSize to already written size - this var holds the absolut number in all fragments
        nLengthTmp1 += nRemainingItemsVar1

        // resize output array and copy data
        if (nLengthTmp1 >= 1) && (nLengthTmp1 <= asn1SizeMax) then
            val newArr: Array[UByte] = Array.fill(nLengthTmp1.toInt)(0)
            arrayCopyOffsetLen(arr, newArr, 0, 0, newArr.length)
            return SomeMut(newArr)
        else
            return NoneMut()

    NoneMut()
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


def BitStream_DecodeOctetString(pBitStrm: BitStream, asn1SizeMin: Long, asn1SizeMax: Long): OptionMut[Array[UByte]] = {

    if asn1SizeMax < 65536 then
        var nCount: Int = 0
        if asn1SizeMin != asn1SizeMax then
            BitStream_DecodeConstraintWholeNumber(pBitStrm, asn1SizeMin, asn1SizeMax) match
                case None() => return NoneMut()
                case Some(l) => nCount = l.toInt
        else
            nCount = asn1SizeMin.toInt

        if (nCount >= asn1SizeMin && nCount <= asn1SizeMax) then
            return BitStream_DecodeOctetString_no_length(pBitStrm, nCount)
        else
            return NoneMut()

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


def BitStream_DecodeBitString(pBitStrm: BitStream, asn1SizeMin: Long, asn1SizeMax: Long): OptionMut[Array[UByte]] = {
    require(asn1SizeMax <= Int.MaxValue)

    if (asn1SizeMax < 65536) {
        var nCount: Long = 0
        if asn1SizeMin != asn1SizeMax then
            BitStream_DecodeConstraintWholeNumber(pBitStrm, asn1SizeMin, asn1SizeMax) match
                case None() => return NoneMut()
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
            case None() => return NoneMut()
            case Some(l) => nRemainingItemsVar1 = l

        val arr: Array[UByte] = Array.fill(asn1SizeMax.toInt)(0)
        while (nRemainingItemsVar1 & 0xC0) == 0xC0 do
            decreases(asn1SizeMax - nCurOffset1) // TODO: check experimental decrease
            if nRemainingItemsVar1 == 0xC4 then
                nCurBlockSize1 = 0x10000
            else if nRemainingItemsVar1 == 0xC3 then
                nCurBlockSize1 = 0xC000
            else if nRemainingItemsVar1 == 0xC2 then
                nCurBlockSize1 = 0x8000
            else if nRemainingItemsVar1 == 0xC1 then
                nCurBlockSize1 = 0x4000
            else
                return NoneMut()

            /*COVERAGE_IGNORE*/
            if nCurOffset1 + nCurBlockSize1 > asn1SizeMax then
                return NoneMut()
            /*COVERAGE_IGNORE*/

            BitStream_ReadBits(pBitStrm, nCurBlockSize1.toInt) match
                case NoneMut() => return NoneMut()
                case SomeMut(t) =>
                    arrayCopyOffsetLen(t, arr, 0, (nCurOffset1 / 8).toInt, nCurBlockSize1.toInt)
                    nLengthTmp1 += nCurBlockSize1
                    nCurOffset1 += nCurBlockSize1
                    BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 0xFF) match
                        case None() => return NoneMut()
                        case Some(l) => nRemainingItemsVar1 = l

        if (nRemainingItemsVar1 & 0x80) > 0 then
            nRemainingItemsVar1 <<= 8
            BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, 0xFF) match
                case None() => return NoneMut()
                case Some(l) =>
                    nRemainingItemsVar1 |= l
                    nRemainingItemsVar1 &= 0x7FFF

        if (nCurOffset1 + nRemainingItemsVar1 <= asn1SizeMax) then

            BitStream_ReadBits(pBitStrm, nRemainingItemsVar1.toInt) match
                case NoneMut() => return NoneMut()
                case SomeMut(t) =>
                    arrayCopyOffsetLen(t, arr, 0, (nCurOffset1 / 8).toInt, nRemainingItemsVar1.toInt)
                    nLengthTmp1 += nRemainingItemsVar1
                    if (nLengthTmp1 >= 1) && (nLengthTmp1 <= asn1SizeMax) then
                        return SomeMut(arr)
    }
    return NoneMut()
}
