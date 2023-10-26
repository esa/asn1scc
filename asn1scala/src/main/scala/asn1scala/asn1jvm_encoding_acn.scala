package asn1scala

import stainless.lang.{None => None, Option => Option, Some => Some}

val FAILED_READ_ERR_CODE = 5400

def Acn_AlignToNextByte(pBitStrm: BitStream, bEncode: Boolean): Unit =
{
    if pBitStrm.currentBit != 0 then
        pBitStrm.currentBit = 0
        pBitStrm.currentByte += 1
        CHECK_BIT_STREAM(pBitStrm)
}

def Acn_AlignToNextWord(pBitStrm: BitStream, bEncode: Boolean): Unit =
{
    Acn_AlignToNextByte(pBitStrm, bEncode)

    pBitStrm.currentByte += pBitStrm.currentByte % 2
    CHECK_BIT_STREAM(pBitStrm)
}

def Acn_AlignToNextDWord(pBitStrm: BitStream, bEncode: Boolean): Unit =
{
    Acn_AlignToNextByte(pBitStrm, bEncode)

    //pBitStrm.currentByte += pBitStrm.currentByte % 4
    val totalBytes: Int = pBitStrm.currentByte % 4
    if pBitStrm.currentByte + totalBytes < pBitStrm.buf.length then
        pBitStrm.currentByte += totalBytes
    else
        val extraBytes: Int = pBitStrm.currentByte + totalBytes - pBitStrm.buf.length
        pBitStrm.currentByte = pBitStrm.buf.length
        pBitStrm.currentByte = extraBytes

    CHECK_BIT_STREAM(pBitStrm)
}

/*ACN Integer functions*/
def Acn_Enc_Int_PositiveInteger_ConstSize(pBitStrm: BitStream, intVal: ULong, encodedSizeInBits: Int): Unit =
{
    if encodedSizeInBits == 0 then
        return

    /* Get number of bits*/
    val nBits: Int = GetNumberOfBitsForNonNegativeInteger(intVal)
    /* put required zeros*/
    BitStream_AppendNBitZero(pBitStrm, encodedSizeInBits - nBits)
    /*Encode number */
    BitStream_EncodeNonNegativeInteger(pBitStrm, intVal)

    CHECK_BIT_STREAM(pBitStrm)
}

def Acn_Enc_Int_PositiveInteger_ConstSize_8(pBitStrm: BitStream, intVal: ULong): Unit =
{
    BitStream_AppendByte0(pBitStrm, intVal.toByte)
    CHECK_BIT_STREAM(pBitStrm)
}

def Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_B(pBitStrm: BitStream, intVal: ULong, size: Int): Unit =
{
    val tmp: ULong = intVal
    var mask: ULong = 0xFF
    mask <<= (size - 1) * 8

    var i: Int = 0
    while i < size do
        val ByteToEncode: Byte = ((tmp & mask) >>> ((size - i - 1) * 8)).toByte
        BitStream_AppendByte0(pBitStrm, ByteToEncode)
        mask >>>= 8
        i += 1

    CHECK_BIT_STREAM(pBitStrm)
}

def Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16(pBitStrm: BitStream, intVal: ULong): Unit =
{
    Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_B(pBitStrm, intVal, 2)
}

def Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm: BitStream, intVal: ULong): Unit =
{
    Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_B(pBitStrm, intVal, 4)
}

def Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64(pBitStrm: BitStream, intVal: ULong): Unit =
{
    Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_B(pBitStrm, intVal, NO_OF_BYTES_IN_JVM_LONG)
}

def Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm: BitStream, intVal: ULong, size: Int): Unit =
{
    var tmp: ULong = intVal

    var i: Int = 0
    while i < size do
        val ByteToEncode: Byte = tmp.toByte
        BitStream_AppendByte0(pBitStrm, ByteToEncode)
        tmp >>>= 8
        i += 1

    CHECK_BIT_STREAM(pBitStrm)
}

def Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16(pBitStrm: BitStream, intVal: ULong): Unit =
{
    Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, intVal, 2)
}

def Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm: BitStream, intVal: ULong): Unit =
{
    Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, intVal, 4)
}

def Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm: BitStream, intVal: ULong): Unit =
{
    Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, intVal, NO_OF_BYTES_IN_JVM_LONG)
}


def Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm: BitStream, encodedSizeInBits: Int): Option[ULong] =
{
    BitStream_DecodeNonNegativeInteger(pBitStrm, encodedSizeInBits) match
        case None() => None()
        case Some(ul) => Some(ul)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_8(pBitStrm: BitStream): Option[ULong] =
{
    BitStream_ReadByte(pBitStrm) match
        case None() => None()
        case Some(ub) => Some(ub & 0xFF)
}

def Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_N(pBitStrm: BitStream, SizeInBytes: Int): Option[ULong] =
{
    var ret: ULong = 0

    var i: Int = 0
    while i < SizeInBytes do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) =>
                ret <<= 8
                ret |= (ub & 0xFF)
        i += 1

    Some(ret)
}

def Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16(pBitStrm: BitStream): Option[ULong] =
{
    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_N(pBitStrm, 2)
}

def Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm: BitStream): Option[ULong] =
{
    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_N(pBitStrm, 4)
}

def Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64(pBitStrm: BitStream): Option[ULong] =
{
    pBitStrm.currentByte += (8 - NO_OF_BYTES_IN_JVM_LONG)

    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_N(pBitStrm, NO_OF_BYTES_IN_JVM_LONG)
}

def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm: BitStream, SizeInBytes: Int): Option[ULong] =
{
    var ret: ULong = 0
    var tmp: ULong = 0

    var i: Int = 0
    while i < SizeInBytes do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) =>
                tmp = ub & 0xFF
                tmp <<= i * 8
                ret |= tmp
        i += 1

    Some(ret)
}

def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16(pBitStrm: BitStream): Option[ULong] =
{
    Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, 2)
}

def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm: BitStream): Option[ULong] =
{
    Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, 4)
}

def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm: BitStream): Option[ULong] =
{
    val ret = Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, NO_OF_BYTES_IN_JVM_LONG)
    pBitStrm.currentByte += (8 - NO_OF_BYTES_IN_JVM_LONG)
    ret
}


def Encode_UnsignedInteger(pBitStrm: BitStream, v: ULong, nBytes: Byte): Unit =
{
    val MAX_BYTE_MASK = 0xFF00000000000000L
    assert(nBytes <= 8)

    var vv: ULong = v << (NO_OF_BYTES_IN_JVM_LONG * 8 -nBytes * 8)

    var i: Int = 0
    while i < nBytes do
        val ByteToEncode: Byte = ((vv & MAX_BYTE_MASK) >>> ((NO_OF_BYTES_IN_JVM_LONG - 1) * 8)).toByte
        BitStream_AppendByte0(pBitStrm, ByteToEncode)
        vv <<= 8
        i += 1
}


def Acn_Enc_Int_PositiveInteger_VarSize_LengthEmbedded(pBitStrm: BitStream, intVal: ULong): Unit =
{
    val nBytes: Byte = GetLengthInBytesOfUInt(intVal).toByte

    /* encode length */
    BitStream_AppendByte0(pBitStrm, nBytes)
    /* Encode integer data*/
    Encode_UnsignedInteger(pBitStrm, intVal, nBytes)

    CHECK_BIT_STREAM(pBitStrm)
}

def Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded(pBitStrm: BitStream): Option[ULong] =
{
    var v: ULong = 0

    BitStream_ReadByte(pBitStrm) match
        case None() => return None()
        case Some(nBytes) =>
            var i: Int = 0
            while i < nBytes do
                BitStream_ReadByte(pBitStrm) match
                    case None() => return None()
                    case Some(ub) =>
                        v = (v << 8) | (ub & 0xFF)
                i += 1

    Some(v)
}


def Acn_Enc_Int_TwosComplement_ConstSize(pBitStrm: BitStream, intVal: Long, encodedSizeInBits: Int): Unit =
{
    if intVal >= 0 then
        BitStream_AppendNBitZero(pBitStrm, encodedSizeInBits - GetNumberOfBitsForNonNegativeInteger(intVal))
        BitStream_EncodeNonNegativeInteger(pBitStrm, intVal)

    else
        BitStream_AppendNBitOne(pBitStrm, encodedSizeInBits - GetNumberOfBitsForNonNegativeInteger(-intVal - 1))
        BitStream_EncodeNonNegativeIntegerNeg(pBitStrm, -intVal - 1, true)

    CHECK_BIT_STREAM(pBitStrm)
}


def Acn_Enc_Int_TwosComplement_ConstSize_8(pBitStrm: BitStream, intVal: Long): Unit =
{
    Acn_Enc_Int_PositiveInteger_ConstSize_8(pBitStrm, int2uint(intVal))
}

def Acn_Enc_Int_TwosComplement_ConstSize_big_endian_16(pBitStrm: BitStream, intVal: Long): Unit =
{
    Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16(pBitStrm, int2uint(intVal))
}

def Acn_Enc_Int_TwosComplement_ConstSize_big_endian_32(pBitStrm: BitStream, intVal: Long): Unit =
{
    Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm, int2uint(intVal))
}

def Acn_Enc_Int_TwosComplement_ConstSize_big_endian_64(pBitStrm: BitStream, intVal: Long): Unit =
{
    Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64(pBitStrm, int2uint(intVal))
}

def Acn_Enc_Int_TwosComplement_ConstSize_little_endian_16(pBitStrm: BitStream, intVal: Long): Unit =
{
    Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16(pBitStrm, int2uint(intVal))
}

def Acn_Enc_Int_TwosComplement_ConstSize_little_endian_32(pBitStrm: BitStream, intVal: Long): Unit =
{
    Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm, int2uint(intVal))
}

def Acn_Enc_Int_TwosComplement_ConstSize_little_endian_64(pBitStrm: BitStream, intVal: Long): Unit =
{
    Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm, int2uint(intVal))
}

def Acn_Dec_Int_TwosComplement_ConstSize(pBitStrm: BitStream, encodedSizeInBits: Int): Option[Long] =
{
    val valIsNegative: Boolean = BitStream_PeekBit(pBitStrm)
    val nBytes: Int = encodedSizeInBits / 8
    val rstBits: Int = encodedSizeInBits % 8

    var pIntVal: Long = if valIsNegative then Long.MaxValue else 0

    var i: Int = 0
    while i < nBytes do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) =>
                pIntVal = (pIntVal << 8) | (ub & 0xFF)
        i += 1

    if rstBits > 0 then
        BitStream_ReadPartialByte(pBitStrm, rstBits.toByte) match
            case None() => return None()
            case Some(ub) =>
                pIntVal = (pIntVal << rstBits) | (ub & 0xFF)

    Some(pIntVal)
}


def Acn_Dec_Int_TwosComplement_ConstSize_8(pBitStrm: BitStream): Option[Long] =
{
    Acn_Dec_Int_PositiveInteger_ConstSize_8(pBitStrm) match
        case None() => None()
        case Some(ul) => Some(uint2int(ul, 1))
}

def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16(pBitStrm: BitStream): Option[Long] =
{
    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16(pBitStrm) match
        case None() => None()
        case Some(ul) => Some(uint2int(ul, 2))
}

def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32(pBitStrm: BitStream): Option[Long] =
{
    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm) match
        case None() => None()
        case Some(ul) => Some(uint2int(ul, 4))
}

def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64(pBitStrm: BitStream): Option[Long] =
{
    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64(pBitStrm) match
        case None() => None()
        case Some(ul) => Some(uint2int(ul, NO_OF_BYTES_IN_JVM_LONG))
}

def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16(pBitStrm: BitStream): Option[Long] =
{
    Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16(pBitStrm) match
        case None() => None()
        case Some(ul) => Some(uint2int(ul, 2))
}

def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32(pBitStrm: BitStream): Option[Long] =
{
    Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm) match
        case None() => None()
        case Some(ul) => Some(uint2int(ul, 4))
}

def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64(pBitStrm: BitStream): Option[Long] =
{
    Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm) match
        case None() => None()
        case Some(ul) => Some(uint2int(ul, NO_OF_BYTES_IN_JVM_LONG))
}


def Acn_Enc_Int_TwosComplement_VarSize_LengthEmbedded(pBitStrm: BitStream, intVal: Long): Unit =
{
    val nBytes: Byte = GetLengthInBytesOfSInt(intVal).toByte

    /* encode length */
    BitStream_AppendByte0(pBitStrm, nBytes)
    /* Encode integer data*/
    Encode_UnsignedInteger(pBitStrm, int2uint(intVal), nBytes)

    CHECK_BIT_STREAM(pBitStrm)
}


def Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded(pBitStrm: BitStream): Option[Long] =
{
    var v: ULong = 0
    var isNegative: Boolean = false

    BitStream_ReadByte(pBitStrm) match
        case None() => None()
        case Some(nBytes) =>
            var i: Int = 0
            while i < nBytes do
                BitStream_ReadByte(pBitStrm) match
                    case None() => return None()
                    case Some(ub) =>
                        if i == 0 && (ub & 0x80) > 0 then
                            v = Long.MaxValue
                            isNegative = true

                        v = (v << 8) | (ub & 0xFF)
                        i += 1

            if isNegative then
                Some(-(~v) - 1)
            else
                Some(v)
}


//return values is in nibbles
def Acn_Get_Int_Size_BCD(intVal: ULong): Int =
{
    var intVar = intVal
    var ret: Int = 0
    while intVar > 0 do
        intVar /= 10
        ret += 1

    ret
}


def Acn_Enc_Int_BCD_ConstSize(pBitStrm: BitStream, intVal: ULong, encodedSizeInNibbles: Int): Unit =
{
    var intVar = intVal
    var totalNibbles: Int = 0
    val tmp: Array[UByte] = Array.fill(100)(0)

    assert(100 >= encodedSizeInNibbles)

    while intVar > 0 do
        tmp(totalNibbles) = (intVar % 10).asInstanceOf[UByte]
        totalNibbles += 1
        intVar /= 10

    assert(encodedSizeInNibbles >= totalNibbles)

    var i: Int = encodedSizeInNibbles - 1
    while i >= 0 do
        BitStream_AppendPartialByte(pBitStrm, tmp(i).toByte, 4,false)
        i -= 1

    CHECK_BIT_STREAM(pBitStrm)
}


def Acn_Dec_Int_BCD_ConstSize(pBitStrm: BitStream, encodedSizeInNibbles: Int): Option[ULong] =
{
    var ret: ULong = 0

    var encodedSizeInNibblesVar = encodedSizeInNibbles
    while encodedSizeInNibblesVar > 0 do
        BitStream_ReadPartialByte(pBitStrm, 4) match
            case None() => return None()
            case Some(digit) =>
                ret *= 10
                ret += digit
        encodedSizeInNibblesVar -= 1

    Some(ret)
}


def Acn_Enc_Int_BCD_VarSize_LengthEmbedded(pBitStrm: BitStream, intVal: ULong): Unit =
{
    val nNibbles: Int = Acn_Get_Int_Size_BCD(intVal)
    /* encode length */
    BitStream_AppendByte0(pBitStrm, nNibbles.toByte)

    /* Encode Number */
    Acn_Enc_Int_BCD_ConstSize(pBitStrm, intVal, nNibbles)

    CHECK_BIT_STREAM(pBitStrm)
}


def Acn_Dec_Int_BCD_VarSize_LengthEmbedded(pBitStrm: BitStream): Option[ULong] =
{
    BitStream_ReadByte(pBitStrm) match
        case None() => None()
        case Some(nNibbles) => Acn_Dec_Int_BCD_ConstSize(pBitStrm, nNibbles)
}


//encoding puts an 'F' at the end
def Acn_Enc_Int_BCD_VarSize_NullTerminated(pBitStrm: BitStream, intVal: ULong): Unit =
{

    val nNibbles: Int = Acn_Get_Int_Size_BCD(intVal)

    /* Encode Number */
    Acn_Enc_Int_BCD_ConstSize(pBitStrm, intVal, nNibbles)

    BitStream_AppendPartialByte(pBitStrm, 0xF, 4, false)

    CHECK_BIT_STREAM(pBitStrm)
}

def Acn_Dec_Int_BCD_VarSize_NullTerminated(pBitStrm: BitStream): Option[ULong] =
{
    var ret: ULong = 0

    while true do
        BitStream_ReadPartialByte(pBitStrm, 4) match
            case None() => return None()
            case Some(digit) =>
                if (digit > 9)
                    return Some(ret)

                ret *= 10
                ret += digit

    Some(ret)
}


def Acn_Enc_UInt_ASCII_ConstSize(pBitStrm: BitStream, intVal: ULong, encodedSizeInBytes: Int): Unit =
{
    var intVar = intVal
    var totalNibbles: Int = 0
    val tmp: Array[UByte] = Array.fill(100)(0)

    assert(100 >= encodedSizeInBytes)

    while intVar > 0 do
        tmp(totalNibbles) = (intVar % 10).asInstanceOf[UByte]
        totalNibbles += 1
        intVar /= 10

    assert(encodedSizeInBytes >= totalNibbles)

    var i = encodedSizeInBytes - 1
    while i >= 0 do
        BitStream_AppendByte0(pBitStrm, (tmp(i) + '0').toByte)
        i -= 1

    CHECK_BIT_STREAM(pBitStrm)
}


def Acn_Enc_SInt_ASCII_ConstSize(pBitStrm: BitStream, intVal: Long, encodedSizeInBytes: Int): Unit =
{
    val absIntVal: ULong = if intVal >= 0 then intVal else -intVal

    /* encode sign */
    BitStream_AppendByte0(pBitStrm, if intVal >= 0 then '+' else '-')

    Acn_Enc_UInt_ASCII_ConstSize(pBitStrm, absIntVal, encodedSizeInBytes-1)
}

def Acn_Dec_UInt_ASCII_ConstSize(pBitStrm: BitStream, encodedSizeInBytes: Int): Option[ULong] =
{
    var encodedSizeInBytesVar = encodedSizeInBytes
    var ret: ULong = 0

    while encodedSizeInBytesVar > 0 do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(digit) =>
                assert(digit >= '0' && digit <= '9')

                ret *= 10
                ret += (digit.toInt -'0').toByte

        encodedSizeInBytesVar -= 1

    Some(ret)
}

def Acn_Dec_SInt_ASCII_ConstSize(pBitStrm: BitStream, encodedSizeInBytes: Int): Option[Long] =
{
    BitStream_ReadByte(pBitStrm) match
        case None() => None()
        case Some(digit) =>
            var sign: Int = 1
            if digit == '+' then
                sign = 1
            else if digit == '-' then
                sign = -1
            else
                assert(false)

            Acn_Dec_UInt_ASCII_ConstSize(pBitStrm, encodedSizeInBytes - 1) match
                case None() => None()
                case Some(ul) => Some(sign * ul)
}


def getIntegerDigits (intVal: ULong): (Array[Byte], Byte) = {
    var intVar = intVal
    val digitsArray100: Array[Byte] = Array.fill(100)(0)
    val reversedDigitsArray: Array[Byte] = Array.fill(100)(0)
    var totalDigits: Byte = 0


    if intVar > 0 then
        while intVar > 0 && totalDigits < 100 do
            reversedDigitsArray(totalDigits) = ('0' + (intVar % 10)).toByte
            totalDigits = (totalDigits + 1).toByte
            intVar /= 10

        var i: Int = totalDigits -1
        while i >= 0 do
            digitsArray100(totalDigits-1 - i) = reversedDigitsArray(i)
            i -= 1

    else
        digitsArray100(0) = '0'
        totalDigits = 1

    (digitsArray100, totalDigits)
}


def Acn_Enc_SInt_ASCII_VarSize_LengthEmbedded(pBitStrm: BitStream, intVal: Long): Unit =
{
    val absIntVal: ULong = if intVal >= 0 then intVal else -intVal
    val (digitsArray100, nChars) = getIntegerDigits(absIntVal)

    /* encode length, plus 1 for sign */
    BitStream_AppendByte0(pBitStrm, (nChars + 1).toByte)

    /* encode sign */
    BitStream_AppendByte0(pBitStrm, if intVal >= 0 then '+' else '-')

    /* encode digits */
    var i: Int = 0
    while i < 100 && digitsArray100(i) != 0x0 do
        BitStream_AppendByte0(pBitStrm, digitsArray100(i))
        i += 1

    CHECK_BIT_STREAM(pBitStrm)
}

def Acn_Enc_UInt_ASCII_VarSize_LengthEmbedded(pBitStrm: BitStream, intVal: ULong): Unit =
{
    val (digitsArray100, nChars) = getIntegerDigits(intVal)

    /* encode length */
    BitStream_AppendByte0(pBitStrm, nChars)
    /* encode digits */
    var i: Int = 0
    while i < 100 && digitsArray100(i) != 0x0 do
        BitStream_AppendByte0(pBitStrm, digitsArray100(i))
        i += 1

    CHECK_BIT_STREAM(pBitStrm)
}


def Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded(pBitStrm: BitStream): Option[ULong] =
{
    BitStream_ReadByte(pBitStrm) match
        case None() => None()
        case Some(nChars) => Acn_Dec_UInt_ASCII_ConstSize(pBitStrm, nChars)
}

def Acn_Dec_SInt_ASCII_VarSize_LengthEmbedded(pBitStrm: BitStream): Option[Long] =
{
    BitStream_ReadByte(pBitStrm) match
        case None() => None()
        case Some(nChars) => Acn_Dec_SInt_ASCII_ConstSize(pBitStrm, nChars)
}


def Acn_Enc_UInt_ASCII_VarSize_NullTerminated(pBitStrm: BitStream, intVal: ULong, null_characters: Array[Byte], null_characters_size: Int): Unit =
{
    val (digitsArray100, nChars) = getIntegerDigits(intVal)

    var i: Int = 0 // TODO: size_t?
    while i < 100 && digitsArray100(i) != 0x0 do
        BitStream_AppendByte0(pBitStrm, digitsArray100(i))
        i += 1

    i = 0
    while i < null_characters_size do
        BitStream_AppendByte0(pBitStrm, null_characters(i))
        i += 1

    CHECK_BIT_STREAM(pBitStrm)
}

def Acn_Enc_SInt_ASCII_VarSize_NullTerminated(pBitStrm: BitStream, intVal: Long, null_characters: Array[Byte], null_characters_size: Int): Unit =
{
    val absValue: ULong = if intVal >= 0 then intVal else -intVal
    BitStream_AppendByte0(pBitStrm, if intVal >= 0 then '+' else '-')

    Acn_Enc_UInt_ASCII_VarSize_NullTerminated(pBitStrm, absValue, null_characters, null_characters_size)
}

/*
flag Acn_Dec_String_Ascii_Null_Teminated_mult(BitStream* pBitStrm, max: Long, null_character: Array[Byte], null_character_size: Int,     char* strVal)
{
byte tmp[10];
size_t sz = null_character_size < 10 ? null_character_size : 10;
val tmp: Array[Byte] = Array.fill(10)(0)
memset(strVal, 0x0, (size_t)max + 1);
//read null_character_size characters into the tmp buffer
for (int j = 0; j < (int)null_character_size; j++) {
if (!BitStream_ReadByte(pBitStrm, &(tmp[j])))
return FALSE;
}

asn1SccSint i = 0;
while (i <= max && (memcmp(null_character, tmp, sz) != 0)) {
strVal[i] = tmp[0];
i++;
for (int j = 0; j < (int)null_character_size - 1; j++)
tmp[j] = tmp[j + 1];
if (!BitStream_ReadByte(pBitStrm, &(tmp[null_character_size - 1])))
return FALSE;
}

strVal[i] = 0x0;
return memcmp(null_character, tmp, sz) == 0;

}


*/


def Acn_Dec_UInt_ASCII_VarSize_NullTerminated(pBitStrm: BitStream, null_characters: Array[Byte], null_characters_size: Int): Option[ULong] =
{
    var digit: Byte = 0
    var ret: ULong = 0
    val tmp: Array[Byte] = Array.fill(10)(0)

    val sz: Int = if null_characters_size < 10 then null_characters_size else 10

    //read null_character_size characters into the tmp buffer
    var j: Int = 0
    while j < null_characters_size do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => tmp(j) = ub
        j += 1

    var i: Long = 0
    while !null_characters.sameElements(tmp) do
        digit = tmp(0)
        i += 1

        j = 0
        while j < null_characters_size - 1 do
            tmp(j) = tmp(j + 1)
            j += 1

        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => tmp(null_characters_size - 1) = ub

        digit = (digit - '0').toByte

        ret *= 10
        ret += digit

    Some(ret)
}


def Acn_Dec_SInt_ASCII_VarSize_NullTerminated(pBitStrm: BitStream, null_characters: Array[Byte], null_characters_size: Int): Option[Long] =
{
    var isNegative: Boolean = false

    BitStream_ReadByte(pBitStrm) match
        case None() => None()
        case Some(digit) =>
            assert(digit == '-' || digit == '+')
            if digit == '-' then
                isNegative = true

            Acn_Dec_UInt_ASCII_VarSize_NullTerminated(pBitStrm, null_characters, null_characters_size) match
                case None() => None()
                case Some(ul) => Some(if isNegative then -ul else ul)
}


/* Boolean Decode */

def BitStream_ReadBitPattern(pBitStrm: BitStream, patternToRead: Array[Byte], nBitsToRead: Int): Option[Boolean] =
{
    val nBytesToRead: Int = nBitsToRead / 8
    val nRemainingBitsToRead: Int = nBitsToRead % 8

    var pBoolValue: Boolean = true
    var i: Int = 0
    while i < nBytesToRead do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(curByte) =>
                if curByte != patternToRead(i) then
                    pBoolValue = false
        i += 1

    if nRemainingBitsToRead > 0 then
        BitStream_ReadPartialByte(pBitStrm, nRemainingBitsToRead.toByte) match
            case None() => return None()
            case Some(curByte) =>
                if curByte != ((patternToRead(nBytesToRead) & 0xFF) >>> (8 - nRemainingBitsToRead)) then
                    pBoolValue = false

    Some(pBoolValue)
}


def BitStream_ReadBitPattern_ignore_value(pBitStrm: BitStream, nBitsToRead: Int): Either[ErrorCode, Int] =
{
    val nBytesToRead: Int = nBitsToRead / 8
    val nRemainingBitsToRead: Int = nBitsToRead % 8

    var i: Int = 0
    while i < nBytesToRead do
        BitStream_ReadByte(pBitStrm) match
            case None() => return Left(FAILED_READ_ERR_CODE)
            case Some(_) => i += 1

    if nRemainingBitsToRead > 0 then
        if BitStream_ReadPartialByte(pBitStrm, nRemainingBitsToRead.toByte).isEmpty then
            return Left(FAILED_READ_ERR_CODE)

    Right(0)
}


/*Real encoding functions*/
def Acn_Enc_Real_IEEE754_32_big_endian(pBitStrm: BitStream, realValue: Float): Unit =
{
    val b: Array[Byte] = java.nio.ByteBuffer.allocate(4).putFloat(realValue).array

    var i: Int = 0
    while i < 4 do
        BitStream_AppendByte0(pBitStrm, b(i))
        i += 1
}

def Acn_Dec_Real_IEEE754_32_big_endian(pBitStrm: BitStream): Option[Double] =
{
    val b: Array[Byte] = Array.fill(4)(0)
    var i: Int = 0
    while i < 4 do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => b(i) = ub
        i += 1

    val dat1 = BigInt(b).toInt
    Some(java.lang.Float.intBitsToFloat(dat1).toDouble)
}

def Acn_Dec_Real_IEEE754_32_big_endian_fp32(pBitStrm: BitStream): Option[Float] =
{
    val b: Array[Byte] = Array.fill(4)(0)
    var i: Int = 0
    while i < 4 do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => b(i) = ub
        i += 1

    val dat1 = BigInt(b).toInt
    Some(java.lang.Float.intBitsToFloat(dat1))
}


def Acn_Enc_Real_IEEE754_64_big_endian(pBitStrm: BitStream, realValue: Double): Unit =
{
    val b: Array[Byte] = java.nio.ByteBuffer.allocate(8).putDouble(realValue).array

    var i: Int = 0
    while i < 8 do
        BitStream_AppendByte0(pBitStrm, b(i))
        i += 1
}

def Acn_Dec_Real_IEEE754_64_big_endian(pBitStrm: BitStream): Option[Double] =
{
    val b: Array[Byte] = Array.fill(8)(0)
    var i: Int = 0
    while i < 8 do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => b(i) = ub
        i += 1

    val dat1 = BigInt(b).toLong
    Some(java.lang.Double.longBitsToDouble(dat1))
}


def Acn_Enc_Real_IEEE754_32_little_endian(pBitStrm: BitStream, realValue: Double): Unit =
{
    val b: Array[Byte] = java.nio.ByteBuffer.allocate(4).putFloat(realValue.toFloat).array

    var i: Int = 3
    while i >= 0 do
        BitStream_AppendByte0(pBitStrm, b(i))
        i -= 1
}

def Acn_Dec_Real_IEEE754_32_little_endian(pBitStrm: BitStream): Option[Double] =
{
    val b: Array[Byte] = Array.fill(4)(0)
    var i: Int = 3
    while i >= 0 do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => b(i) = ub
                i -= 1

    val dat1 = BigInt(b).toInt
    Some(java.lang.Float.intBitsToFloat(dat1).toDouble)

}
def Acn_Dec_Real_IEEE754_32_little_endian_fp32(pBitStrm: BitStream): Option[Float] =
{
    val b: Array[Byte] = Array.fill(4)(0)
    var i: Int = 3
    while i >= 0 do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => b(i) = ub
                i -= 1

    val dat1 = BigInt(b).toInt
    Some(java.lang.Float.intBitsToFloat(dat1))
}

def Acn_Enc_Real_IEEE754_64_little_endian(pBitStrm: BitStream, realValue: Double): Unit =
{
    val b: Array[Byte] = java.nio.ByteBuffer.allocate(8).putDouble(realValue).array

    var i: Int = 7
    while i >= 0 do
        BitStream_AppendByte0(pBitStrm, b(i))
        i -= 1
}

def Acn_Dec_Real_IEEE754_64_little_endian(pBitStrm: BitStream): Option[Double] =
{
    val b: Array[Byte] = Array.fill(8)(0)
    var i: Int = 7
    while i >= 0 do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => b(i) = ub
                i -= 1

    val dat1 = BigInt(b).toLong
    Some(java.lang.Double.longBitsToDouble(dat1))
}




/* String functions*/
def Acn_Enc_String_Ascii_FixSize(pBitStrm: BitStream, max: Long, strVal: Array[ASCIIChar]): Unit =
{
    var i: Long = 0
    while i < max do
        BitStream_AppendByte(pBitStrm, strVal(i.toInt), false)
        i += 1
}
def Acn_Enc_String_Ascii_private(pBitStrm: BitStream, max: Long, strVal: Array[ASCIIChar]): Long =
{
    var i: Long = 0
    while (i < max) && (strVal(i.toInt) != '\u0000') do
        BitStream_AppendByte(pBitStrm, strVal(i.toInt), false)
        i += 1

    i
}

def Acn_Enc_String_Ascii_Null_Teminated(pBitStrm: BitStream, max: Long, null_character: Byte, strVal: Array[ASCIIChar]): Unit =
{
    Acn_Enc_String_Ascii_private(pBitStrm, max, strVal)
    BitStream_AppendByte(pBitStrm, null_character.toByte, false)
}

def Acn_Enc_String_Ascii_Null_Teminated_mult(pBitStrm: BitStream, max: Long, null_character: Array[Byte], null_character_size: Int, strVal: Array[ASCIIChar]): Unit =
{
    Acn_Enc_String_Ascii_private(pBitStrm, max, strVal)
    var i: Int = 0
    while i < null_character_size do
        BitStream_AppendByte(pBitStrm, null_character(i), false)
        i += 1
}


def Acn_Enc_String_Ascii_External_Field_Determinant(pBitStrm: BitStream, max: Long, strVal: Array[ASCIIChar]): Unit =
{
    Acn_Enc_String_Ascii_private(pBitStrm, max, strVal)
}

def Acn_Enc_String_Ascii_Internal_Field_Determinant(pBitStrm: BitStream, max: Long, min: Long, strVal: Array[ASCIIChar]): Unit =
{
    val strLen: Int = strVal.length
    BitStream_EncodeConstraintWholeNumber(pBitStrm, if strLen <= max then strLen else max, min, max)
    Acn_Enc_String_Ascii_private(pBitStrm, max, strVal)
}

def Acn_Enc_String_CharIndex_FixSize(pBitStrm: BitStream, max: Long, allowedCharSet: Array[Byte], strVal: Array[ASCIIChar]): Unit =
{
    var i: Int = 0
    while i < max do
        val charIndex: Int = GetCharIndex(strVal(i), allowedCharSet)
        BitStream_EncodeConstraintWholeNumber(pBitStrm, charIndex, 0, allowedCharSet.length - 1)
        i += 1
}

def Acn_Enc_String_CharIndex_private(pBitStrm: BitStream, max: Long, allowedCharSet: Array[Byte], strVal: Array[ASCIIChar]): Long =
{
    var i: Int = 0
    while (i < max) && (strVal(i) != '\u0000') do
        val charIndex: Int = GetCharIndex(strVal(i), allowedCharSet)
        BitStream_EncodeConstraintWholeNumber(pBitStrm, charIndex, 0, allowedCharSet.length - 1)
        i += 1

    i
}


def Acn_Enc_String_CharIndex_External_Field_Determinant (pBitStrm: BitStream, max: Long, allowedCharSet: Array[Byte], strVal: Array[ASCIIChar]): Unit =
{
    Acn_Enc_String_CharIndex_private(pBitStrm, max, allowedCharSet, strVal)
}

def Acn_Enc_String_CharIndex_Internal_Field_Determinant (pBitStrm: BitStream, max: Long, allowedCharSet: Array[Byte], min: Long, strVal: Array[ASCIIChar]): Unit =
{
    val strLen: Int = strVal.length
    BitStream_EncodeConstraintWholeNumber(pBitStrm, if strLen <= max then strLen else max, min, max)
    Acn_Enc_String_CharIndex_private(pBitStrm, max, allowedCharSet, strVal)
}


def Acn_Enc_IA5String_CharIndex_External_Field_Determinant(pBitStrm: BitStream, max: Long, strVal: Array[ASCIIChar]): Unit =
{
    val allowedCharSet: Array[ASCIIChar] = Array(
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
        0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13,
        0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
        0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
        0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
        0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
        0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
        0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
        0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
        0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, 0x62, 0x63,
        0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D,
        0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
        0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
    )

    Acn_Enc_String_CharIndex_private(pBitStrm, max, allowedCharSet, strVal)
}

def Acn_Enc_IA5String_CharIndex_Internal_Field_Determinant(pBitStrm: BitStream, max: Long, min: Long, strVal: Array[ASCIIChar]): Unit =
{
    val allowedCharSet: Array[ASCIIChar] = Array(
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
        0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13,
        0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
        0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
        0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
        0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
        0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
        0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
        0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
        0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, 0x62, 0x63,
        0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D,
        0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
        0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
    )
    val strLen: Int = strVal.length
    BitStream_EncodeConstraintWholeNumber(pBitStrm, if strLen <= max then strLen else max, min, max)
    Acn_Enc_String_CharIndex_private(pBitStrm, max, allowedCharSet, strVal)
}


def Acn_Dec_String_Ascii_private(pBitStrm: BitStream, max: Long, charactersToDecode: Long): Option[Array[ASCIIChar]] =
{
    val strVal: Array[ASCIIChar] = Array.fill(max.toInt+1)(0)
    var i: Int = 0
    while i < charactersToDecode do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(decodedCharacter) =>
                strVal(i) = decodedCharacter
        i += 1
    Some(strVal)
}


def Acn_Dec_String_Ascii_FixSize(pBitStrm: BitStream, max: Long): Option[Array[ASCIIChar]] =
{
    Acn_Dec_String_Ascii_private(pBitStrm, max, max)
}

/*
int put_byte_in_last_dec_bytes(byte last_dec_bytes[], size_t* pCur_size, size_t null_characters_size, byte decodedCharacter, byte *pDiscardedCharacter) {
int i;
if (*pCur_size < null_characters_size) {
last_dec_bytes[*pCur_size] = decodedCharacter;
(*pCur_size)++;
*pDiscardedCharacter = NULL;
return 0;
} else {
*pDiscardedCharacter = last_dec_bytes[0];
for (i = 1; i < null_characters_size; i++) {
last_dec_bytes[i - 1] = last_dec_bytes[i];
}
last_dec_bytes[null_characters_size - 1] = decodedCharacter;
return 1;
}
}

flag Acn_Dec_String_Ascii_Null_Teminated(BitStream* pBitStrm, max: Long, const byte null_characters[], size_t null_characters_size, char* strVal)
{
asn1SccSint i = 0;
byte decodedCharacter;
byte characterToAppendInString;
size_t cur_size_of_last_dec_bytes = 0;
byte last_dec_bytes[128];
int ret;

assert(null_characters_size<128);
memset(last_dec_bytes, 0x0, sizeof(last_dec_bytes));
memset(strVal, 0x0, (size_t)max+1);
while (i<=max) {
if (!BitStream_ReadByte(pBitStrm, &decodedCharacter))
return FALSE;
ret = put_byte_in_last_dec_bytes(last_dec_bytes, &cur_size_of_last_dec_bytes, null_characters_size, decodedCharacter, &characterToAppendInString);


//if (decodedCharacter == (byte)null_character) {
if ((ret == 1) && (memcmp(last_dec_bytes,null_characters,null_characters_size) == 0)) {
strVal[i] = 0x0;
return TRUE;
} else if (ret == 1) {
strVal[i] = characterToAppendInString;
i++;
}
}

return FALSE;

}

*/


def Acn_Dec_String_Ascii_Null_Teminated(pBitStrm: BitStream, max: Long, null_character: ASCIIChar): Option[Array[ASCIIChar]] =
{
    val strVal: Array[ASCIIChar] = Array.fill(max.toInt+1)(0)
    var i: Int = 0
    while i <= max do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(decodedCharacter) =>
                if decodedCharacter != null_character then
                    strVal(i) = decodedCharacter
                    i += 1
                else
                    strVal(i) = 0x0
                    return Some(strVal)

    None()

}
def Acn_Dec_String_Ascii_Null_Teminated_mult(pBitStrm: BitStream, max: Long, null_character: Array[ASCIIChar], null_character_size: Int): Option[Array[ASCIIChar]] =
{
    val sz: Int = if null_character_size < 10 then null_character_size else 10
    val tmp: Array[Byte] = Array.fill(10)(0)
    val strVal: Array[ASCIIChar] = Array.fill(max.toInt+1)(0)
    //read null_character_size characters into the tmp buffer
    var j: Int = 0
    while j < null_character_size do
        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => tmp(j) = ub
        j += 1


    var i: Int = 0
    while i <= max && !null_character.sameElements(tmp) do
        strVal(i) = tmp(0)
        i += 1
        j = 0
        while j < null_character_size - 1 do
            tmp(j) = tmp(j + 1)
            j += 1

        BitStream_ReadByte(pBitStrm) match
            case None() => return None()
            case Some(ub) => tmp(null_character_size - 1) = ub

    strVal(i) = 0x0

    if !null_character.sameElements(tmp) then
        return None()

    Some(strVal)
}


def Acn_Dec_String_Ascii_External_Field_Determinant(pBitStrm: BitStream, max: Long, extSizeDeterminatFld: Long): Option[Array[ASCIIChar]] =
{
    Acn_Dec_String_Ascii_private(pBitStrm, max, if extSizeDeterminatFld <= max then extSizeDeterminatFld else max)
}

def Acn_Dec_String_Ascii_Internal_Field_Determinant(pBitStrm: BitStream, max: Long, min: Long): Option[Array[ASCIIChar]] =
{
    BitStream_DecodeConstraintWholeNumber(pBitStrm, min, max) match
        case None() => None()
        case Some(nCount) =>
            Acn_Dec_String_Ascii_private(pBitStrm, max, if nCount <= max then nCount else max)
}

def Acn_Dec_String_CharIndex_private(pBitStrm: BitStream, max: Long, charactersToDecode: Long, allowedCharSet: Array[Byte]): Option[Array[ASCIIChar]] =
{
    val strVal: Array[ASCIIChar] = Array.fill(max.toInt+1)(0)
    var i: Int = 0
    while i < charactersToDecode do
        BitStream_DecodeConstraintWholeNumber(pBitStrm, 0, allowedCharSet.length - 1) match
            case None() => return None()
            case Some(charIndex) =>
                strVal(i) = allowedCharSet(charIndex.toInt)
        i += 1

    Some(strVal)
}

def Acn_Dec_String_CharIndex_FixSize (pBitStrm: BitStream, max: Long, allowedCharSet: Array[ASCIIChar]): Option[Array[ASCIIChar]] =
{
    Acn_Dec_String_CharIndex_private(pBitStrm, max, max, allowedCharSet)
}

def Acn_Dec_String_CharIndex_External_Field_Determinant (pBitStrm: BitStream, max: Long, allowedCharSet: Array[ASCIIChar], extSizeDeterminatFld: Long): Option[Array[ASCIIChar]] =
{
    Acn_Dec_String_CharIndex_private(pBitStrm, max, if extSizeDeterminatFld <= max then extSizeDeterminatFld else max, allowedCharSet)
}


def Acn_Dec_String_CharIndex_Internal_Field_Determinant (pBitStrm: BitStream, max: Long, allowedCharSet: Array[ASCIIChar], min: Long): Option[Array[ASCIIChar]] =
{
    BitStream_DecodeConstraintWholeNumber(pBitStrm, min, max) match
        case None() => None()
        case Some(nCount) =>
            Acn_Dec_String_CharIndex_private(pBitStrm, max, if nCount <= max then nCount else max, allowedCharSet)
}


def Acn_Dec_IA5String_CharIndex_External_Field_Determinant(pBitStrm: BitStream, max: Long, extSizeDeterminatFld: Long): Option[Array[ASCIIChar]] =
{
    val allowedCharSet: Array[ASCIIChar] = Array(
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
        0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13,
        0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
        0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
        0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
        0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
        0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
        0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
        0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
        0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, 0x62, 0x63,
        0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D,
        0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
        0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
    )
    Acn_Dec_String_CharIndex_private(pBitStrm, max, if extSizeDeterminatFld <= max then extSizeDeterminatFld else max, allowedCharSet)
}

def Acn_Dec_IA5String_CharIndex_Internal_Field_Determinant(pBitStrm: BitStream, max: Long, min: Long): Option[Array[ASCIIChar]] =
{
    val allowedCharSet: Array[ASCIIChar] = Array(
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
        0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13,
        0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
        0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
        0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
        0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
        0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
        0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
        0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
        0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, 0x62, 0x63,
        0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D,
        0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
        0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
    )
    BitStream_DecodeConstraintWholeNumber(pBitStrm, min, max) match
        case None() => None()
        case Some(nCount) =>
            Acn_Dec_String_CharIndex_private(pBitStrm, max, if nCount <= max then nCount else max, allowedCharSet)
}




/* Length Determinant functions*/
def Acn_Enc_Length(pBitStrm: BitStream, lengthValue: ULong, lengthSizeInBits: Int): Unit =
{
    /* encode length */
    Acn_Enc_Int_PositiveInteger_ConstSize(pBitStrm, lengthValue, lengthSizeInBits)
}

def Acn_Dec_Length(pBitStrm: BitStream, lengthSizeInBits: Int): Option[ULong] =
{
    Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm, lengthSizeInBits)
}

def milbus_encode(v: Long): Long =
{
    if v == 32 then 0 else v
}

def milbus_decode(v: Long): Long =
{
    if v == 0 then 32 else v
}


def Acn_Dec_Int_PositiveInteger_ConstSizeUInt8 (pBitStrm: BitStream, encodedSizeInBits: Int): Option[UByte] = {
    Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm, encodedSizeInBits) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_PositiveInteger_ConstSizeUInt16 (pBitStrm: BitStream, encodedSizeInBits: Int): Option[UShort] = {
    Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm, encodedSizeInBits) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_PositiveInteger_ConstSizeUInt32 (pBitStrm: BitStream, encodedSizeInBits: Int): Option[UInt] = {
    Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm, encodedSizeInBits) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}

def Acn_Dec_Int_PositiveInteger_ConstSize_8UInt8 (pBitStrm: BitStream): Option[UByte] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_8(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16UInt16 (pBitStrm: BitStream): Option[UShort] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16UInt8 (pBitStrm: BitStream): Option[UByte] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt32 (pBitStrm: BitStream): Option[UInt] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt16 (pBitStrm: BitStream): Option[UShort] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}

def Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt8 (pBitStrm: BitStream): Option[UByte] = {
    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt32 (pBitStrm: BitStream): Option[UInt] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt16 (pBitStrm: BitStream): Option[UShort] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt8 (pBitStrm: BitStream): Option[UByte] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16UInt16 (pBitStrm: BitStream): Option[UShort] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16UInt8 (pBitStrm: BitStream): Option[UByte] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt32 (pBitStrm: BitStream): Option[UInt] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt16 (pBitStrm: BitStream): Option[UShort] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt8 (pBitStrm: BitStream): Option[UByte] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt32 (pBitStrm: BitStream): Option[UInt] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt16 (pBitStrm: BitStream): Option[UShort] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt8 (pBitStrm: BitStream): Option[UByte] =    {
    Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt8 (pBitStrm: BitStream): Option[UByte] =    {
    Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt16 (pBitStrm: BitStream): Option[UShort] =    {
    Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt32 (pBitStrm: BitStream): Option[UInt] =    {
    Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_Int_TwosComplement_ConstSizeInt8 (pBitStrm: BitStream, encodedSizeInBits: Int): Option[Byte] = {
    Acn_Dec_Int_TwosComplement_ConstSize(pBitStrm, encodedSizeInBits) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_TwosComplement_ConstSizeInt16 (pBitStrm: BitStream, encodedSizeInBits: Int): Option[Short] = {
    Acn_Dec_Int_TwosComplement_ConstSize(pBitStrm, encodedSizeInBits) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_TwosComplement_ConstSizeInt32 (pBitStrm: BitStream, encodedSizeInBits: Int): Option[Int] = {
    Acn_Dec_Int_TwosComplement_ConstSize(pBitStrm, encodedSizeInBits) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_Int_TwosComplement_ConstSize_8Int8 (pBitStrm: BitStream): Option[Byte] = {
    Acn_Dec_Int_TwosComplement_ConstSize_8(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16Int16 (pBitStrm: BitStream): Option[Short] = {
    Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16Int8 (pBitStrm: BitStream): Option[Byte] = {
    Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int32 (pBitStrm: BitStream): Option[Int] = {
    Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int16 (pBitStrm: BitStream): Option[Short] = {
    Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int8 (pBitStrm: BitStream): Option[Byte] = {
    Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int32 (pBitStrm: BitStream): Option[Int] = {
    Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int16 (pBitStrm: BitStream): Option[Short] = {
    Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int8 (pBitStrm: BitStream): Option[Byte] = {
    Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16Int16 (pBitStrm: BitStream): Option[Short] = {
    Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16Int8 (pBitStrm: BitStream): Option[Byte] = {
    Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int32 (pBitStrm: BitStream): Option[Int] = {
    Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int16 (pBitStrm: BitStream): Option[Short] = {
    Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int8 (pBitStrm: BitStream): Option[Byte] = {
    Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int32 (pBitStrm: BitStream): Option[Int] = {
    Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int16 (pBitStrm: BitStream): Option[Short] = {
    Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int8 (pBitStrm: BitStream): Option[Byte] = {
    Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_TwosComplement_VarSize_LengthEmbeddedInt8 (pBitStrm: BitStream): Option[Byte] = {
    Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_TwosComplement_VarSize_LengthEmbeddedInt16 (pBitStrm: BitStream): Option[Short] = {
    Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_TwosComplement_VarSize_LengthEmbeddedInt32 (pBitStrm: BitStream): Option[Int] = {
    Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_Int_BCD_ConstSizeUInt8 (pBitStrm: BitStream, encodedSizeInNibbles: Int): Option[UByte] = {
    Acn_Dec_Int_BCD_ConstSize(pBitStrm, encodedSizeInNibbles) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_BCD_ConstSizeUInt16 (pBitStrm: BitStream, encodedSizeInNibbles: Int): Option[UShort] = {
    Acn_Dec_Int_BCD_ConstSize(pBitStrm, encodedSizeInNibbles) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_BCD_ConstSizeUInt32 (pBitStrm: BitStream, encodedSizeInNibbles: Int): Option[UInt] = {
    Acn_Dec_Int_BCD_ConstSize(pBitStrm, encodedSizeInNibbles) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_Int_BCD_VarSize_LengthEmbeddedUInt8 (pBitStrm: BitStream): Option[UByte] = {
    Acn_Dec_Int_BCD_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_BCD_VarSize_LengthEmbeddedUInt16 (pBitStrm: BitStream): Option[UShort] = {
    Acn_Dec_Int_BCD_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_BCD_VarSize_LengthEmbeddedUInt32 (pBitStrm: BitStream): Option[UInt] = {
    Acn_Dec_Int_BCD_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt8 (pBitStrm: BitStream): Option[UByte] = {
    Acn_Dec_Int_BCD_VarSize_NullTerminated(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt16 (pBitStrm: BitStream): Option[UShort] = {
    Acn_Dec_Int_BCD_VarSize_NullTerminated(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt32 (pBitStrm: BitStream): Option[UInt] = {
    Acn_Dec_Int_BCD_VarSize_NullTerminated(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_SInt_ASCII_ConstSizeInt8 (pBitStrm: BitStream, encodedSizeInBytes: Int): Option[Byte] = {
    Acn_Dec_SInt_ASCII_ConstSize(pBitStrm, encodedSizeInBytes) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_SInt_ASCII_ConstSizeInt16 (pBitStrm: BitStream, encodedSizeInBytes: Int): Option[Short] = {
    Acn_Dec_SInt_ASCII_ConstSize(pBitStrm, encodedSizeInBytes) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_SInt_ASCII_ConstSizeInt32 (pBitStrm: BitStream, encodedSizeInBytes: Int): Option[Int] = {
    Acn_Dec_SInt_ASCII_ConstSize(pBitStrm, encodedSizeInBytes) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_SInt_ASCII_VarSize_LengthEmbeddedInt8 (pBitStrm: BitStream): Option[Byte] = {
    Acn_Dec_SInt_ASCII_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_SInt_ASCII_VarSize_LengthEmbeddedInt16 (pBitStrm: BitStream): Option[Short] = {
    Acn_Dec_SInt_ASCII_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_SInt_ASCII_VarSize_LengthEmbeddedInt32 (pBitStrm: BitStream): Option[Int] = {
    Acn_Dec_SInt_ASCII_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_SInt_ASCII_VarSize_NullTerminatedInt8 (pBitStrm: BitStream, null_characters: Array[Byte], null_characters_size: Int): Option[Byte] = {
    Acn_Dec_SInt_ASCII_VarSize_NullTerminated(pBitStrm, null_characters, null_characters_size) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_SInt_ASCII_VarSize_NullTerminatedInt16 (pBitStrm: BitStream, null_characters: Array[Byte], null_characters_size: Int): Option[Short] = {
    Acn_Dec_SInt_ASCII_VarSize_NullTerminated(pBitStrm, null_characters, null_characters_size) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_SInt_ASCII_VarSize_NullTerminatedInt32 (pBitStrm: BitStream, null_characters: Array[Byte], null_characters_size: Int): Option[Int] = {
    Acn_Dec_SInt_ASCII_VarSize_NullTerminated(pBitStrm, null_characters, null_characters_size) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_UInt_ASCII_ConstSizeUInt8 (pBitStrm: BitStream, encodedSizeInBytes: Int): Option[UByte] = {
    Acn_Dec_UInt_ASCII_ConstSize(pBitStrm, encodedSizeInBytes) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_UInt_ASCII_ConstSizeUInt16 (pBitStrm: BitStream, encodedSizeInBytes: Int): Option[UShort] = {
    Acn_Dec_UInt_ASCII_ConstSize(pBitStrm, encodedSizeInBytes) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_UInt_ASCII_ConstSizeUInt32 (pBitStrm: BitStream, encodedSizeInBytes: Int): Option[UInt] = {
    Acn_Dec_UInt_ASCII_ConstSize(pBitStrm, encodedSizeInBytes) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_UInt_ASCII_VarSize_LengthEmbeddedUInt8 (pBitStrm: BitStream): Option[UByte] = {
    Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_UInt_ASCII_VarSize_LengthEmbeddedUInt16 (pBitStrm: BitStream): Option[UShort] = {
    Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_UInt_ASCII_VarSize_LengthEmbeddedUInt32 (pBitStrm: BitStream): Option[UInt] = {
    Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded(pBitStrm) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}


def Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt8 (pBitStrm: BitStream, null_characters: Array[Byte], null_characters_size: Int): Option[UByte] = {
    Acn_Dec_UInt_ASCII_VarSize_NullTerminated(pBitStrm, null_characters, null_characters_size) match
        case None() => None()
        case Some(v) => Some(v.toByte)
}


def Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt16 (pBitStrm: BitStream, null_characters: Array[Byte], null_characters_size: Int): Option[UShort] = {
    Acn_Dec_UInt_ASCII_VarSize_NullTerminated(pBitStrm, null_characters, null_characters_size) match
        case None() => None()
        case Some(v) => Some(v.toShort)
}


def Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt32 (pBitStrm: BitStream, null_characters: Array[Byte], null_characters_size: Int): Option[UInt] = {
    Acn_Dec_UInt_ASCII_VarSize_NullTerminated(pBitStrm, null_characters, null_characters_size) match
        case None() => None()
        case Some(v) => Some(v.toInt)
}

