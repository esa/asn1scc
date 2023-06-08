package asn1scala


/*
def RequiresReverse(): Boolean =
{
  val word: Short = 0x0001
  char * b = (char *) & word
  return b[0] == 1;
}
*/

def Acn_AlignToNextByte(pBitStrm: BitStream, bEncode: Boolean): Unit =
{
  if pBitStrm.currentBit != 0 then
    pBitStrm.currentBit = 0
    pBitStrm.currentByte += 1
    if bEncode then
      bitstream_push_data_if_required(pBitStrm)
    else
      bitstream_fetch_data_if_required(pBitStrm)
    CHECK_BIT_STREAM(pBitStrm)
}

def Acn_AlignToNextWord(pBitStrm: BitStream, bEncode: Boolean): Unit =
{
  Acn_AlignToNextByte(pBitStrm, bEncode)

  pBitStrm.currentByte += pBitStrm.currentByte % 2
  if bEncode then
    bitstream_push_data_if_required(pBitStrm)
  else
    bitstream_fetch_data_if_required(pBitStrm)

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
    if bEncode then
      bitstream_push_data_if_required(pBitStrm)
    else
      bitstream_fetch_data_if_required(pBitStrm)
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

def Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_B(pBitStrm: BitStream,  intVal: ULong,  size: Int): Unit =
{
  val tmp: ULong = intVal
  var mask: ULong = 0xFF
  mask <<= (size - 1) * 8

  var i: Int = 0
  while i < size do
    val ByteToEncode: Byte = ((tmp & mask) >> ((size - i - 1) * 8)).toByte
    BitStream_AppendByte0(pBitStrm, ByteToEncode)
    mask >>= 8
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
  // Avoid dead code warnings by conditionally compiling this part.
  /*#if WORD_SIZE != 8
  int i;
  for (i = 0; i < 8 - WORD_SIZE; i ++) {
    BitStream_AppendByte0(pBitStrm, 0x0);
  }
  #endif*/
  Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_B(pBitStrm, intVal, WORD_SIZE)
}

def Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm: BitStream, intVal: ULong, size: Int): Unit =
{
  var tmp: ULong = intVal;

  var i: Int = 0
  while i < size do
    val ByteToEncode: Byte = tmp.toByte
    BitStream_AppendByte0(pBitStrm, ByteToEncode)
    tmp >>= 8
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
  Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, intVal, WORD_SIZE)
  // Avoid dead code warnings by conditionally compiling this part.
  /*#if WORD_SIZE != 8
  int i;
  for (i = 0; i < 8 - WORD_SIZE; i ++)
  {
    BitStream_AppendByte0(pBitStrm, 0x0);
  }
  #endif*/
}


def Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm: BitStream, encodedSizeInBits: Int): Option[ULong] =
{
  BitStream_DecodeNonNegativeInteger(pBitStrm, encodedSizeInBits) match
    case None => None
    case Some(ul) => Some(ul)
}


def Acn_Dec_Int_PositiveInteger_ConstSize_8(pBitStrm: BitStream): Option[ULong] =
{
  BitStream_ReadByte(pBitStrm) match
    case None => None
    case Some(ub) => Some(ub)
}

def Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_N(pBitStrm: BitStream, SizeInBytes: Int): Option[ULong] =
{
  var ret: ULong = 0

  var i: Int = 0
  while i < SizeInBytes do
    BitStream_ReadByte(pBitStrm) match
      case None => return None
      case Some(ub) =>
        ret <<= 8
        ret |= ub
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
  pBitStrm.currentByte += (8 - WORD_SIZE)

  Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_N(pBitStrm, WORD_SIZE)
}

def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm: BitStream, SizeInBytes: Int): Option[ULong] =
{
  var ret: ULong = 0
  var tmp: ULong = 0

  var i: Int = 0
  while i < SizeInBytes do
    BitStream_ReadByte(pBitStrm) match
      case None => return None
      case Some(ub) =>
        tmp = ub
        tmp <<= i * 8
        ret |= tmp
    i += 1

  return Some(ret)
}

def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16(pBitStrm: BitStream): Option[ULong] =
{
  return Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, 2);
}

def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm: BitStream): Option[ULong] =
{
  return Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, 4);
}

def Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm: BitStream): Option[ULong] =
{
  val ret = Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, WORD_SIZE)
  pBitStrm.currentByte += (8 - WORD_SIZE)
  ret
}


def Encode_UnsignedInteger(pBitStrm: BitStream, v: ULong, nBytes: Byte): Unit =
{
  val MAX_BYTE_MASK = if WORD_SIZE == 8 then 0xFF00000000000000L else 0xFF000000
  assert(nBytes <= 8)

  var vv: ULong = v << (WORD_SIZE * 8 -nBytes * 8)

  var i: Int = 0
  while i < nBytes do
    val ByteToEncode: Byte = ((vv & MAX_BYTE_MASK) >> ((WORD_SIZE - 1) * 8)).toByte
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
    case None => return None
    case Some(nBytes) =>
      var i: Int = 0
      while i < nBytes do
        BitStream_ReadByte(pBitStrm) match
          case None => return None
          case Some(ub) =>
            v = (v << 8) | ub
        i += 1

  Some(v)
}


def Acn_Enc_Int_TwosComplement_ConstSize(pBitStrm: BitStream, intVal: Long, encodedSizeInBits: Int): Unit =
{
  if intVal >= 0 then
    BitStream_AppendNBitZero(pBitStrm, encodedSizeInBits - GetNumberOfBitsForNonNegativeInteger(intVal))
    BitStream_EncodeNonNegativeInteger(pBitStrm, intVal)

  else
    BitStream_AppendNBitOne(pBitStrm, encodedSizeInBits - GetNumberOfBitsForNonNegativeInteger((-intVal - 1)))
    BitStream_EncodeNonNegativeIntegerNeg(pBitStrm, (-intVal - 1), true)

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
      case None => return None
      case Some(ub) =>
        pIntVal = (pIntVal << 8) | ub
    i += 1

  if rstBits > 0 then
    BitStream_ReadPartialByte(pBitStrm, rstBits.toByte) match
      case None => return None
      case Some(ub) =>
        pIntVal = (pIntVal << rstBits) | ub

  return Some(pIntVal)
}


def Acn_Dec_Int_TwosComplement_ConstSize_8(pBitStrm: BitStream): Option[Long] =
{
  Acn_Dec_Int_PositiveInteger_ConstSize_8(pBitStrm) match
    case None => None
    case Some(ul) => Some(uint2int(ul, 1))
}

def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16(pBitStrm: BitStream): Option[Long] =
{
  Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16(pBitStrm) match
    case None => None
    case Some(ul) => Some(uint2int(ul, 2))
}

def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32(pBitStrm: BitStream): Option[Long] =
{
  Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm) match
    case None => None
    case Some(ul) => Some(uint2int(ul, 4))
}

def Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64(pBitStrm: BitStream): Option[Long] =
{
  Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64(pBitStrm) match
    case None => None
    case Some(ul) => Some(uint2int(ul, WORD_SIZE))
}

def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16(pBitStrm: BitStream): Option[Long] =
{
  Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16(pBitStrm) match
    case None => None
    case Some(ul) => Some(uint2int(ul, 2))
}

def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32(pBitStrm: BitStream): Option[Long] =
{
  Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm) match
    case None => None
    case Some(ul) => Some(uint2int(ul, 4))
}

def Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64(pBitStrm: BitStream): Option[Long] =
{
  Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm) match
    case None => None
    case Some(ul) => Some(uint2int(ul, WORD_SIZE))
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
    case None => return None
    case Some(nBytes) =>
      var i: Int = 0
      while i < nBytes do
        BitStream_ReadByte(pBitStrm) match
          case None => return None
          case Some(ub) =>
            if i == 0 && (ub & 0x80) > 0 then
              v = Long.MaxValue
              isNegative = true

            v = (v << 8) | ub
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
  val tmp: Array[Byte] = Array.fill(100)(0)

  assert(100 >= encodedSizeInNibbles);

  while intVar > 0 do
    tmp(totalNibbles) = (intVar % 10).toByte
    totalNibbles += 1
    intVar /= 10

  assert(encodedSizeInNibbles >= totalNibbles)

  var i: Int = encodedSizeInNibbles - 1
  while i >= 0 do
    BitStream_AppendPartialByte(pBitStrm, tmp(i), 4,false)
    i -= 1

  CHECK_BIT_STREAM(pBitStrm)
}


def Acn_Dec_Int_BCD_ConstSize(pBitStrm: BitStream, encodedSizeInNibbles: Int): Option[ULong] =
{
  var ret: ULong = 0

  var encodedSizeInNibblesVar = encodedSizeInNibbles
  while encodedSizeInNibblesVar > 0 do
    BitStream_ReadPartialByte(pBitStrm, 4) match
      case None => return None
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
    case None => None
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
      case None => return None
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
  val tmp: Array[Byte] = Array.fill(100)(0)

  assert(100 >= encodedSizeInBytes);

  while intVar > 0 do
    tmp(totalNibbles) = (intVar % 10).toByte
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

/*
flag Acn_Dec_UInt_ASCII_ConstSize(pBitStrm: BitStream, asn1SccUint * pIntVal, int encodedSizeInBytes)
{
  byte digit;
  asn1SccUint ret = 0;

  while (encodedSizeInBytes > 0) {
    if (!BitStream_ReadByte(pBitStrm, & digit))
      return FALSE;
    ASSERT_OR_RETURN_FALSE(digit >= '0' && digit <= '9');
    digit = (byte)((int) digit -'0');

    ret *= 10;
    ret += digit;

    encodedSizeInBytes --;
  }
  * pIntVal = ret;

  return TRUE;
}

flag Acn_Dec_SInt_ASCII_ConstSize(pBitStrm: BitStream, asn1SccSint * pIntVal, int encodedSizeInBytes)
{
  byte digit;
  asn1SccUint ret = 0;
  int sign = 1;

  if (!BitStream_ReadByte(pBitStrm, & digit))
    return FALSE;
  if (digit == '+')
    sign = 1;
  else if (digit == '-')
    sign = -1;
  else {
    ASSERT_OR_RETURN_FALSE(0);
  }
  encodedSizeInBytes --;


  if (!Acn_Dec_UInt_ASCII_ConstSize(pBitStrm, & ret, encodedSizeInBytes)) {
    return false;
  }

  * pIntVal = (asn1SccSint) ret;

  * pIntVal = sign * (* pIntVal);
  return TRUE;

}


void getIntegerDigits (intVal: UInt, byte digitsArray100[]
, byte * totalDigits
);

void getIntegerDigits (intVal: UInt, byte digitsArray100[]
, byte * totalDigits
) {
  int i = 0;
  * totalDigits = 0;
  byte reversedDigitsArray
  [
  100
  ];
  memset(reversedDigitsArray, 0x0, 100);
  memset(digitsArray100, 0x0, 100);
  if (intVal > 0) {
    while (intVal > 0 && * totalDigits < 100
    )
    {
      reversedDigitsArray[* totalDigits
      ] = '0' + (byte)(intVal % 10);
      (* totalDigits) ++;
      intVal /= 10;
    }
    for (i
    = * totalDigits -1;
    i >= 0;
    i --
    )
    {
      digitsArray100[(* totalDigits -1) - i] = reversedDigitsArray[i];
    }
  }
  else {
    digitsArray100[0] = '0';
    * totalDigits = 1;
  }
}


void Acn_Enc_SInt_ASCII_VarSize_LengthEmbedded(pBitStrm: BitStream, asn1SccSint intVal)
{
  byte digitsArray100
  [
  100
  ];
  int i = 0;
  byte nChars;
  asn1SccUint absIntVal = intVal >= 0 ? (asn1SccUint) intVal: (asn1SccUint)
  (-intVal);
  getIntegerDigits(absIntVal, digitsArray100, & nChars);

  /* encode length, plus 1 for sign */
  BitStream_AppendByte0(pBitStrm, nChars + 1);

  /* encode sign */
  BitStream_AppendByte0(pBitStrm, intVal >= 0 ? '+': '-');

  /* encode digits */
  while (i < 100 && digitsArray100[i] != 0x0) {
    BitStream_AppendByte0(pBitStrm, digitsArray100[i]);
    i ++;
  }

  CHECK_BIT_STREAM(pBitStrm);

}

void Acn_Enc_UInt_ASCII_VarSize_LengthEmbedded(pBitStrm: BitStream, intVal: UInt)
{
  byte digitsArray100
  [
  100
  ];
  int i = 0;
  byte nChars;
  getIntegerDigits(intVal, digitsArray100, & nChars);

  /* encode length */
  BitStream_AppendByte0(pBitStrm, nChars);
  /* encode digits */
  while (i < 100 && digitsArray100[i] != 0x0) {
    BitStream_AppendByte0(pBitStrm, digitsArray100[i]);
    i ++;
  }

  CHECK_BIT_STREAM(pBitStrm);

}


flag Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded(pBitStrm: BitStream, asn1SccUint * pIntVal)
{
  byte nChars = 0;
  if (BitStream_ReadByte(pBitStrm, & nChars))
    return Acn_Dec_UInt_ASCII_ConstSize(pBitStrm, pIntVal, nChars);

  return FALSE;
}

flag Acn_Dec_SInt_ASCII_VarSize_LengthEmbedded(pBitStrm: BitStream, asn1SccSint * pIntVal)
{
  byte nChars = 0;
  if (BitStream_ReadByte(pBitStrm, & nChars))
    return Acn_Dec_SInt_ASCII_ConstSize(pBitStrm, pIntVal, nChars);

  return FALSE;
}


void Acn_Enc_UInt_ASCII_VarSize_NullTerminated(pBitStrm: BitStream, intVal: UInt, const byte null_characters[], size_t null_characters_size)
{
  byte digitsArray100
  [
  100
  ];
  byte nChars;
  size_t i = 0;
  getIntegerDigits(intVal, digitsArray100, & nChars);
  while (i < 100 && digitsArray100[i] != 0x0) {
    BitStream_AppendByte0(pBitStrm, digitsArray100[i]);
    i ++;
  }
  for (i
  = 0;
  i < null_characters_size;
  i ++
  )
  BitStream_AppendByte0(pBitStrm, null_characters[i]);
  CHECK_BIT_STREAM(pBitStrm);
}

void Acn_Enc_SInt_ASCII_VarSize_NullTerminated(pBitStrm: BitStream, asn1SccSint intVal, const byte null_characters[], size_t null_characters_size)
{
  asn1SccUint absValue = intVal >= 0 ? (asn1SccUint) intVal: (asn1SccUint)
  (-intVal);
  BitStream_AppendByte0(pBitStrm, intVal >= 0 ? '+': '-');

  Acn_Enc_UInt_ASCII_VarSize_NullTerminated(pBitStrm, absValue, null_characters, null_characters_size);
}

/*
flag Acn_Dec_String_Ascii_Null_Teminated_mult(BitStream* pBitStrm, asn1SccSint max, const byte null_character[], size_t null_character_size,   char* strVal)
{
byte tmp[10];
size_t sz = null_character_size < 10 ? null_character_size : 10;
memset(tmp, 0x0, 10);
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

flag Acn_Dec_UInt_ASCII_VarSize_NullTerminated(pBitStrm: BitStream, asn1SccUint * pIntVal, const byte null_characters[], size_t null_characters_size)
{
  byte digit;
  asn1SccUint ret = 0;
  byte tmp
  [
  10
  ];
  size_t sz = null_characters_size < 10 ? null_characters_size: 10;
  memset(tmp, 0x0, 10);
  asn1SccSint i = 0;

  //read null_character_size characters into the tmp buffer
  for (int j
  = 0;
  j < (int) null_characters_size;
  j ++
  )
  {
    if (!BitStream_ReadByte(pBitStrm, &(tmp[j])))
      return FALSE;
  }

  while (memcmp(null_characters, tmp, sz) != 0) {
    digit = tmp[0];
    i ++;
    for (int j
    = 0;
    j < (int) null_characters_size -1;
    j ++
    )
    tmp[j] = tmp[j + 1];
    if (!BitStream_ReadByte(pBitStrm, &(tmp[null_characters_size - 1])))
      return FALSE;

    digit = (byte)((int) digit -'0');

    ret *= 10;
    ret += digit;
  }

  * pIntVal = ret;

  return TRUE;
}


flag Acn_Dec_SInt_ASCII_VarSize_NullTerminated(pBitStrm: BitStream, asn1SccSint * pIntVal, const byte null_characters[], size_t null_characters_size)
{
  byte digit;
  asn1SccUint ret = 0;
  flag isNegative = FALSE;

  if (!BitStream_ReadByte(pBitStrm, & digit))
    return FALSE;
  ASSERT_OR_RETURN_FALSE(digit == '-' || digit == '+');
  if (digit == '-')
    isNegative = TRUE;

  if (!Acn_Dec_UInt_ASCII_VarSize_NullTerminated(pBitStrm, & ret, null_characters, null_characters_size))
    return false;

  * pIntVal = (asn1SccSint) ret;
  if (isNegative)
    * pIntVal = -(* pIntVal);
  return TRUE;
}





/* Boolean Decode */

flag BitStream_ReadBitPattern(pBitStrm: BitStream, const byte * patternToRead, int nBitsToRead, flag * pBoolValue)
{
  int nBytesToRead = nBitsToRead / 8;
  int nRemainingBitsToRead = nBitsToRead % 8;
  byte curByte;
  int i = 0;

  * pBoolValue = TRUE;
  for (i
  = 0;
  i < nBytesToRead;
  i ++
  )
  {
    if (!BitStream_ReadByte(pBitStrm, & curByte))
      return FALSE;
    if (curByte != patternToRead[i])
      * pBoolValue = FALSE;
  }

  if (nRemainingBitsToRead > 0) {
    if (!BitStream_ReadPartialByte(pBitStrm, & curByte, (byte) nRemainingBitsToRead))
      return FALSE;
    if (curByte != patternToRead[nBytesToRead] >> (8 - nRemainingBitsToRead))
      * pBoolValue = FALSE;
  }

  return TRUE;
}

flag BitStream_ReadBitPattern_ignore_value(pBitStrm: BitStream, int nBitsToRead)
{
  int nBytesToRead = nBitsToRead / 8;
  int nRemainingBitsToRead = nBitsToRead % 8;
  byte curByte;
  int i = 0;

  for (i
  = 0;
  i < nBytesToRead;
  i ++
  )
  {
    if (!BitStream_ReadByte(pBitStrm, & curByte))
      return FALSE;
  }

  if (nRemainingBitsToRead > 0) {
    if (!BitStream_ReadPartialByte(pBitStrm, & curByte, (byte) nRemainingBitsToRead))
      return FALSE;
  }

  return TRUE;

}

/*Real encoding functions*/
typedef union _float_tag {
  float f;
  byte b[sizeof (float)
  ];
} _float;

typedef union _double_tag {
  double f;
  byte b[sizeof (double)
  ];
} _double;


#define Acn_enc_real_big_endian(
type) \
int i;
\
_ ##
type dat1;
\
dat1.f = (
type) realValue;
\
if (!RequiresReverse()) {
  \
  for (i
  = 0;
  i < (int) sizeof (dat1);
  i ++
  ) \
  BitStream_AppendByte0(pBitStrm, dat1.b[i]);
  \
} else {
  \
  for (i
  = (int)(sizeof(dat1) - 1);
  i >= 0;
  i --
  ) \
  BitStream_AppendByte0(pBitStrm, dat1.b[i]);
  \
} \


#define Acn_dec_real_big_endian(
type) \
int i;
\
_ ##
type dat1;
\
dat1.f = 0.0;
\
if (!RequiresReverse()) {
  \
  for (i
  = 0;
  i < (int) sizeof (dat1);
  i ++
  )
  {
    \
    if (!BitStream_ReadByte(pBitStrm, & dat1.b[i])
    ) \
    return FALSE;
    \
  } \
} else {
  \
  for (i
  = (int)(sizeof(dat1) - 1);
  i >= 0;
  i --
  )
  {
    \
    if (!BitStream_ReadByte(pBitStrm, & dat1.b[i])
    ) \
    return FALSE;
    \
  } \
} \
  * pRealValue = dat1.f;
\
return TRUE;
\


void Acn_Enc_Real_IEEE754_32_big_endian(pBitStrm: BitStream, asn1Real realValue)
{
  Acn_enc_real_big_endian(float)
}

flag Acn_Dec_Real_IEEE754_32_big_endian(pBitStrm: BitStream, asn1Real * pRealValue)
{
  Acn_dec_real_big_endian(float)
}

flag Acn_Dec_Real_IEEE754_32_big_endian_fp32(pBitStrm: BitStream, float * pRealValue)
{
  Acn_dec_real_big_endian(float)
}


void Acn_Enc_Real_IEEE754_64_big_endian(pBitStrm: BitStream, asn1Real realValue)
{
  Acn_enc_real_big_endian(double)
}

flag Acn_Dec_Real_IEEE754_64_big_endian(pBitStrm: BitStream, asn1Real * pRealValue)
{
  Acn_dec_real_big_endian(double)
}


#define Acn_enc_real_little_endian(
type) \
int i;
\
_ ##
type dat1;
\
dat1.f = (
type) realValue;
\
if (RequiresReverse()) {
  \
  for (i
  = 0;
  i < (int) sizeof (dat1);
  i ++
  ) \
  BitStream_AppendByte0(pBitStrm, dat1.b[i]);
  \
} else {
  \
  for (i
  = (int)(sizeof(dat1) - 1);
  i >= 0;
  i --
  ) \
  BitStream_AppendByte0(pBitStrm, dat1.b[i]);
  \
} \


#define Acn_dec_real_little_endian(
type) \
int i;
\
_ ##
type dat1;
\
dat1.f = 0.0;
\
if (RequiresReverse()) {
  \
  for (i
  = 0;
  i < (int) sizeof (dat1);
  i ++
  )
  {
    \
    if (!BitStream_ReadByte(pBitStrm, & dat1.b[i])
    ) \
    return FALSE;
    \
  } \
} else {
  \
  for (i
  = (int)(sizeof(dat1) - 1);
  i >= 0;
  i --
  )
  {
    \
    if (!BitStream_ReadByte(pBitStrm, & dat1.b[i])
    ) \
    return FALSE;
    \
  } \
} \
  * pRealValue = dat1.f;
\
return TRUE;
\


void Acn_Enc_Real_IEEE754_32_little_endian(pBitStrm: BitStream, asn1Real realValue)
{
  Acn_enc_real_little_endian(float)
}

flag Acn_Dec_Real_IEEE754_32_little_endian(pBitStrm: BitStream, asn1Real * pRealValue)
{
  Acn_dec_real_little_endian(float)
}

flag Acn_Dec_Real_IEEE754_32_little_endian_fp32(pBitStrm: BitStream, float * pRealValue)
{
  Acn_dec_real_little_endian(float)
}

void Acn_Enc_Real_IEEE754_64_little_endian(pBitStrm: BitStream, asn1Real realValue)
{
  Acn_enc_real_little_endian(double)
}

flag Acn_Dec_Real_IEEE754_64_little_endian(pBitStrm: BitStream, asn1Real * pRealValue)
{
  Acn_dec_real_little_endian(double)
}




/* String functions*/
void Acn_Enc_String_Ascii_FixSize(pBitStrm: BitStream, asn1SccSint max, const char * strVal)
{
  asn1SccSint i = 0;
  while (i < max) {
    BitStream_AppendByte(pBitStrm, strVal[i], FALSE);
    i ++;
  }
}

static asn1SccSint Acn_Enc_String_Ascii_private(pBitStrm: BitStream,
  asn1SccSint max,
  const char * strVal)
{
  asn1SccSint i = 0;
  while ((i < max) && (strVal[i] != '\0')) {
    BitStream_AppendByte(pBitStrm, strVal[i], FALSE);
    i ++;
  }
  return i;
}

void Acn_Enc_String_Ascii_Null_Teminated(pBitStrm: BitStream, asn1SccSint max, char null_character, const char * strVal)
{
  Acn_Enc_String_Ascii_private(pBitStrm, max, strVal);
  BitStream_AppendByte(pBitStrm, null_character, FALSE);
}

void Acn_Enc_String_Ascii_Null_Teminated_mult (pBitStrm: BitStream, asn1SccSint max, const byte null_character[], size_t null_character_size, const char * strVal) {
  size_t i = 0;
  Acn_Enc_String_Ascii_private(pBitStrm, max, strVal);
  for (i
  = 0;
  i < null_character_size;
  i ++
  )
  {
    BitStream_AppendByte(pBitStrm, null_character[i], FALSE);
  }
}


void Acn_Enc_String_Ascii_External_Field_Determinant(pBitStrm: BitStream, asn1SccSint max, const char * strVal)
{
  Acn_Enc_String_Ascii_private(pBitStrm, max, strVal);
}

void Acn_Enc_String_Ascii_Internal_Field_Determinant(pBitStrm: BitStream, asn1SccSint max, asn1SccSint min, const char * strVal)
{
  int strLen = (int) strlen (strVal);
  BitStream_EncodeConstraintWholeNumber(pBitStrm, strLen <= max ? strLen: max, min, max);
  Acn_Enc_String_Ascii_private(pBitStrm, max, strVal);
}

void Acn_Enc_String_CharIndex_FixSize (pBitStrm: BitStream, asn1SccSint max, byte allowedCharSet[]
, int charSetSize
, const char * strVal
)
{
  asn1SccSint i = 0;
  while (i < max) {
    int charIndex = GetCharIndex(strVal[i], allowedCharSet, charSetSize);
    BitStream_EncodeConstraintWholeNumber(pBitStrm, charIndex, 0, charSetSize - 1);
    i ++;
  }
}

static asn1SccSint Acn_Enc_String_CharIndex_private(pBitStrm: BitStream,
  asn1SccSint max,
  byte allowedCharSet[]
,
int charSetSize
,
const char * strVal
)
{
  asn1SccSint i = 0;
  while ((i < max) && (strVal[i] != '\0')) {
    int charIndex = GetCharIndex(strVal[i], allowedCharSet, charSetSize);
    BitStream_EncodeConstraintWholeNumber(pBitStrm, charIndex, 0, charSetSize - 1);
    i ++;
  }
  return i;
}


void Acn_Enc_String_CharIndex_External_Field_Determinant (pBitStrm: BitStream, asn1SccSint max, byte allowedCharSet[]
, int charSetSize
, const char * strVal
)
{
  Acn_Enc_String_CharIndex_private(pBitStrm, max, allowedCharSet, charSetSize, strVal);
}

void Acn_Enc_String_CharIndex_Internal_Field_Determinant (pBitStrm: BitStream, asn1SccSint max, byte allowedCharSet[]
, int charSetSize
, asn1SccSint min
, const char * strVal
)
{
  int strLen = (int) strlen (strVal);
  BitStream_EncodeConstraintWholeNumber(pBitStrm, strLen <= max ? strLen: max, min, max);
  Acn_Enc_String_CharIndex_private(pBitStrm, max, allowedCharSet, charSetSize, strVal);
}


void Acn_Enc_IA5String_CharIndex_External_Field_Determinant(pBitStrm: BitStream, asn1SccSint max, const char * strVal)
{
  static byte allowedCharSet[] = {
    0x00
    , 0x01
    , 0x02
    , 0x03
    , 0x04
    , 0x05
    , 0x06
    , 0x07
    , 0x08
    , 0x09
    ,
    0x0A
    , 0x0B
    , 0x0C
    , 0x0D
    , 0x0E
    , 0x0F
    , 0x10
    , 0x11
    , 0x12
    , 0x13
    ,
    0x14
    , 0x15
    , 0x16
    , 0x17
    , 0x18
    , 0x19
    , 0x1A
    , 0x1B
    , 0x1C
    , 0x1D
    ,
    0x1E
    , 0x1F
    , 0x20
    , 0x21
    , 0x22
    , 0x23
    , 0x24
    , 0x25
    , 0x26
    , 0x27
    ,
    0x28
    , 0x29
    , 0x2A
    , 0x2B
    , 0x2C
    , 0x2D
    , 0x2E
    , 0x2F
    , 0x30
    , 0x31
    ,
    0x32
    , 0x33
    , 0x34
    , 0x35
    , 0x36
    , 0x37
    , 0x38
    , 0x39
    , 0x3A
    , 0x3B
    ,
    0x3C
    , 0x3D
    , 0x3E
    , 0x3F
    , 0x40
    , 0x41
    , 0x42
    , 0x43
    , 0x44
    , 0x45
    ,
    0x46
    , 0x47
    , 0x48
    , 0x49
    , 0x4A
    , 0x4B
    , 0x4C
    , 0x4D
    , 0x4E
    , 0x4F
    ,
    0x50
    , 0x51
    , 0x52
    , 0x53
    , 0x54
    , 0x55
    , 0x56
    , 0x57
    , 0x58
    , 0x59
    ,
    0x5A
    , 0x5B
    , 0x5C
    , 0x5D
    , 0x5E
    , 0x5F
    , 0x60
    , 0x61
    , 0x62
    , 0x63
    ,
    0x64
    , 0x65
    , 0x66
    , 0x67
    , 0x68
    , 0x69
    , 0x6A
    , 0x6B
    , 0x6C
    , 0x6D
    ,
    0x6E
    , 0x6F
    , 0x70
    , 0x71
    , 0x72
    , 0x73
    , 0x74
    , 0x75
    , 0x76
    , 0x77
    ,
    0x78
    , 0x79
    , 0x7A
    , 0x7B
    , 0x7C
    , 0x7D
    , 0x7E
    , 0x7F
  };

  Acn_Enc_String_CharIndex_private(pBitStrm, max, allowedCharSet, 128, strVal);
}

void Acn_Enc_IA5String_CharIndex_Internal_Field_Determinant(pBitStrm: BitStream, asn1SccSint max, asn1SccSint min, const char * strVal)
{
  static byte allowedCharSet[] = {
    0x00
    , 0x01
    , 0x02
    , 0x03
    , 0x04
    , 0x05
    , 0x06
    , 0x07
    , 0x08
    , 0x09
    ,
    0x0A
    , 0x0B
    , 0x0C
    , 0x0D
    , 0x0E
    , 0x0F
    , 0x10
    , 0x11
    , 0x12
    , 0x13
    ,
    0x14
    , 0x15
    , 0x16
    , 0x17
    , 0x18
    , 0x19
    , 0x1A
    , 0x1B
    , 0x1C
    , 0x1D
    ,
    0x1E
    , 0x1F
    , 0x20
    , 0x21
    , 0x22
    , 0x23
    , 0x24
    , 0x25
    , 0x26
    , 0x27
    ,
    0x28
    , 0x29
    , 0x2A
    , 0x2B
    , 0x2C
    , 0x2D
    , 0x2E
    , 0x2F
    , 0x30
    , 0x31
    ,
    0x32
    , 0x33
    , 0x34
    , 0x35
    , 0x36
    , 0x37
    , 0x38
    , 0x39
    , 0x3A
    , 0x3B
    ,
    0x3C
    , 0x3D
    , 0x3E
    , 0x3F
    , 0x40
    , 0x41
    , 0x42
    , 0x43
    , 0x44
    , 0x45
    ,
    0x46
    , 0x47
    , 0x48
    , 0x49
    , 0x4A
    , 0x4B
    , 0x4C
    , 0x4D
    , 0x4E
    , 0x4F
    ,
    0x50
    , 0x51
    , 0x52
    , 0x53
    , 0x54
    , 0x55
    , 0x56
    , 0x57
    , 0x58
    , 0x59
    ,
    0x5A
    , 0x5B
    , 0x5C
    , 0x5D
    , 0x5E
    , 0x5F
    , 0x60
    , 0x61
    , 0x62
    , 0x63
    ,
    0x64
    , 0x65
    , 0x66
    , 0x67
    , 0x68
    , 0x69
    , 0x6A
    , 0x6B
    , 0x6C
    , 0x6D
    ,
    0x6E
    , 0x6F
    , 0x70
    , 0x71
    , 0x72
    , 0x73
    , 0x74
    , 0x75
    , 0x76
    , 0x77
    ,
    0x78
    , 0x79
    , 0x7A
    , 0x7B
    , 0x7C
    , 0x7D
    , 0x7E
    , 0x7F
  };
  int strLen = (int) strlen (strVal);
  BitStream_EncodeConstraintWholeNumber(pBitStrm, strLen <= max ? strLen: max, min, max);
  Acn_Enc_String_CharIndex_private(pBitStrm, max, allowedCharSet, 128, strVal);
}


static flag Acn_Dec_String_Ascii_private(pBitStrm: BitStream,
  asn1SccSint max,
  asn1SccSint charactersToDecode,
  char * strVal)
{
  asn1SccSint i = 0;
  byte decodedCharacter;
  memset(strVal, 0x0, (size_t) max +1);
  while (i < charactersToDecode) {
    if (!BitStream_ReadByte(pBitStrm, & decodedCharacter))
      return FALSE;
    strVal[i] = decodedCharacter;
    i ++;
  }
  return TRUE;
}


flag Acn_Dec_String_Ascii_FixSize(pBitStrm: BitStream, asn1SccSint max, char * strVal)
{
  return Acn_Dec_String_Ascii_private(pBitStrm, max, max, strVal);
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

flag Acn_Dec_String_Ascii_Null_Teminated(BitStream* pBitStrm, asn1SccSint max, const byte null_characters[], size_t null_characters_size, char* strVal)
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


flag Acn_Dec_String_Ascii_Null_Teminated(pBitStrm: BitStream, asn1SccSint max, char null_character, char * strVal)
{
  asn1SccSint i = 0;
  byte decodedCharacter;
  memset(strVal, 0x0, (size_t) max +1);
  while (i <= max) {
    if (!BitStream_ReadByte(pBitStrm, & decodedCharacter))
      return FALSE;
    if (decodedCharacter != (byte) null_character) {
      strVal[i] = decodedCharacter;
      i ++;
    }
    else {
      strVal[i] = 0x0;
      return TRUE;
    }
  }

  return FALSE;

}

flag Acn_Dec_String_Ascii_Null_Teminated_mult(pBitStrm: BitStream, asn1SccSint max, const byte null_character[], size_t null_character_size, char * strVal)
{
  byte tmp
  [
  10
  ];
  size_t sz = null_character_size < 10 ? null_character_size: 10;
  memset(tmp, 0x0, 10);
  memset(strVal, 0x0, (size_t) max +1);
  //read null_character_size characters into the tmp buffer
  for (int j
  = 0;
  j < (int) null_character_size;
  j ++
  )
  {
    if (!BitStream_ReadByte(pBitStrm, &(tmp[j])))
      return FALSE;
  }

  asn1SccSint i = 0;
  while (i <= max && (memcmp(null_character, tmp, sz) != 0)) {
    strVal[i] = tmp[0];
    i ++;
    for (int j
    = 0;
    j < (int) null_character_size -1;
    j ++
    )
    tmp[j] = tmp[j + 1];
    if (!BitStream_ReadByte(pBitStrm, &(tmp[null_character_size - 1])))
      return FALSE;
  }

  strVal[i] = 0x0;
  return memcmp(null_character, tmp, sz) == 0;

}


flag Acn_Dec_String_Ascii_External_Field_Determinant(pBitStrm: BitStream, asn1SccSint max, asn1SccSint extSizeDeterminatFld, char * strVal)
{
  return Acn_Dec_String_Ascii_private(pBitStrm, max, extSizeDeterminatFld <= max ? extSizeDeterminatFld: max, strVal);
}

flag Acn_Dec_String_Ascii_Internal_Field_Determinant(pBitStrm: BitStream, asn1SccSint max, asn1SccSint min, char * strVal)
{
  asn1SccSint nCount;
  if (!BitStream_DecodeConstraintWholeNumber(pBitStrm, & nCount, min, max))
    return FALSE;

  return Acn_Dec_String_Ascii_private(pBitStrm, max, nCount <= max ? nCount: max, strVal);

}

static flag Acn_Dec_String_CharIndex_private(pBitStrm: BitStream,
  asn1SccSint max,
  asn1SccSint charactersToDecode,
  byte allowedCharSet[]
,
int charSetSize
,
char * strVal
)
{
  asn1SccSint i = 0;
  memset(strVal, 0x0, (size_t) max +1);
  while (i < charactersToDecode) {
    asn1SccSint charIndex = 0;
    if (!BitStream_DecodeConstraintWholeNumber(pBitStrm, & charIndex, 0, charSetSize - 1))
      return FALSE;
    strVal[i] = allowedCharSet[charIndex];
    i ++;
  }
  return TRUE;
}


flag Acn_Dec_String_CharIndex_FixSize (pBitStrm: BitStream, asn1SccSint max, byte allowedCharSet[]
, int charSetSize
, char * strVal
)
{
  return Acn_Dec_String_CharIndex_private(pBitStrm, max, max, allowedCharSet, charSetSize, strVal);
}

flag Acn_Dec_String_CharIndex_External_Field_Determinant (pBitStrm: BitStream, asn1SccSint max, byte allowedCharSet[]
, int charSetSize
, asn1SccSint extSizeDeterminatFld
, char * strVal
)
{
  return Acn_Dec_String_CharIndex_private(pBitStrm, max, extSizeDeterminatFld <= max ? extSizeDeterminatFld: max, allowedCharSet, charSetSize, strVal);
}

flag Acn_Dec_String_CharIndex_Internal_Field_Determinant (pBitStrm: BitStream, asn1SccSint max, byte allowedCharSet[]
, int charSetSize
, asn1SccSint min
, char * strVal
)
{
  asn1SccSint nCount;
  if (!BitStream_DecodeConstraintWholeNumber(pBitStrm, & nCount, min, max))
    return FALSE;
  return Acn_Dec_String_CharIndex_private(pBitStrm, max, nCount <= max ? nCount: max, allowedCharSet, charSetSize, strVal);
}


flag Acn_Dec_IA5String_CharIndex_External_Field_Determinant(pBitStrm: BitStream, asn1SccSint max, asn1SccSint extSizeDeterminatFld, char * strVal)
{
  static byte allowedCharSet[] = {
    0x00
    , 0x01
    , 0x02
    , 0x03
    , 0x04
    , 0x05
    , 0x06
    , 0x07
    , 0x08
    , 0x09
    ,
    0x0A
    , 0x0B
    , 0x0C
    , 0x0D
    , 0x0E
    , 0x0F
    , 0x10
    , 0x11
    , 0x12
    , 0x13
    ,
    0x14
    , 0x15
    , 0x16
    , 0x17
    , 0x18
    , 0x19
    , 0x1A
    , 0x1B
    , 0x1C
    , 0x1D
    ,
    0x1E
    , 0x1F
    , 0x20
    , 0x21
    , 0x22
    , 0x23
    , 0x24
    , 0x25
    , 0x26
    , 0x27
    ,
    0x28
    , 0x29
    , 0x2A
    , 0x2B
    , 0x2C
    , 0x2D
    , 0x2E
    , 0x2F
    , 0x30
    , 0x31
    ,
    0x32
    , 0x33
    , 0x34
    , 0x35
    , 0x36
    , 0x37
    , 0x38
    , 0x39
    , 0x3A
    , 0x3B
    ,
    0x3C
    , 0x3D
    , 0x3E
    , 0x3F
    , 0x40
    , 0x41
    , 0x42
    , 0x43
    , 0x44
    , 0x45
    ,
    0x46
    , 0x47
    , 0x48
    , 0x49
    , 0x4A
    , 0x4B
    , 0x4C
    , 0x4D
    , 0x4E
    , 0x4F
    ,
    0x50
    , 0x51
    , 0x52
    , 0x53
    , 0x54
    , 0x55
    , 0x56
    , 0x57
    , 0x58
    , 0x59
    ,
    0x5A
    , 0x5B
    , 0x5C
    , 0x5D
    , 0x5E
    , 0x5F
    , 0x60
    , 0x61
    , 0x62
    , 0x63
    ,
    0x64
    , 0x65
    , 0x66
    , 0x67
    , 0x68
    , 0x69
    , 0x6A
    , 0x6B
    , 0x6C
    , 0x6D
    ,
    0x6E
    , 0x6F
    , 0x70
    , 0x71
    , 0x72
    , 0x73
    , 0x74
    , 0x75
    , 0x76
    , 0x77
    ,
    0x78
    , 0x79
    , 0x7A
    , 0x7B
    , 0x7C
    , 0x7D
    , 0x7E
    , 0x7F
  };
  return Acn_Dec_String_CharIndex_private(pBitStrm, max, extSizeDeterminatFld <= max ? extSizeDeterminatFld: max, allowedCharSet, 128, strVal);
}

flag Acn_Dec_IA5String_CharIndex_Internal_Field_Determinant(pBitStrm: BitStream, asn1SccSint max, asn1SccSint min, char * strVal)
{
  asn1SccSint nCount;
  static byte allowedCharSet[] = {
    0x00
    , 0x01
    , 0x02
    , 0x03
    , 0x04
    , 0x05
    , 0x06
    , 0x07
    , 0x08
    , 0x09
    ,
    0x0A
    , 0x0B
    , 0x0C
    , 0x0D
    , 0x0E
    , 0x0F
    , 0x10
    , 0x11
    , 0x12
    , 0x13
    ,
    0x14
    , 0x15
    , 0x16
    , 0x17
    , 0x18
    , 0x19
    , 0x1A
    , 0x1B
    , 0x1C
    , 0x1D
    ,
    0x1E
    , 0x1F
    , 0x20
    , 0x21
    , 0x22
    , 0x23
    , 0x24
    , 0x25
    , 0x26
    , 0x27
    ,
    0x28
    , 0x29
    , 0x2A
    , 0x2B
    , 0x2C
    , 0x2D
    , 0x2E
    , 0x2F
    , 0x30
    , 0x31
    ,
    0x32
    , 0x33
    , 0x34
    , 0x35
    , 0x36
    , 0x37
    , 0x38
    , 0x39
    , 0x3A
    , 0x3B
    ,
    0x3C
    , 0x3D
    , 0x3E
    , 0x3F
    , 0x40
    , 0x41
    , 0x42
    , 0x43
    , 0x44
    , 0x45
    ,
    0x46
    , 0x47
    , 0x48
    , 0x49
    , 0x4A
    , 0x4B
    , 0x4C
    , 0x4D
    , 0x4E
    , 0x4F
    ,
    0x50
    , 0x51
    , 0x52
    , 0x53
    , 0x54
    , 0x55
    , 0x56
    , 0x57
    , 0x58
    , 0x59
    ,
    0x5A
    , 0x5B
    , 0x5C
    , 0x5D
    , 0x5E
    , 0x5F
    , 0x60
    , 0x61
    , 0x62
    , 0x63
    ,
    0x64
    , 0x65
    , 0x66
    , 0x67
    , 0x68
    , 0x69
    , 0x6A
    , 0x6B
    , 0x6C
    , 0x6D
    ,
    0x6E
    , 0x6F
    , 0x70
    , 0x71
    , 0x72
    , 0x73
    , 0x74
    , 0x75
    , 0x76
    , 0x77
    ,
    0x78
    , 0x79
    , 0x7A
    , 0x7B
    , 0x7C
    , 0x7D
    , 0x7E
    , 0x7F
  };
  if (!BitStream_DecodeConstraintWholeNumber(pBitStrm, & nCount, min, max))
    return FALSE;
  return Acn_Dec_String_CharIndex_private(pBitStrm, max, nCount <= max ? nCount: max, allowedCharSet, 128, strVal);
}






/* Length Determinant functions*/
void Acn_Enc_Length(pBitStrm: BitStream, asn1SccUint lengthValue, int lengthSizeInBits)
{
  /* encode length */
  Acn_Enc_Int_PositiveInteger_ConstSize(pBitStrm, lengthValue, lengthSizeInBits);
}

flag Acn_Dec_Length(pBitStrm: BitStream, asn1SccUint * pLengthValue, int lengthSizeInBits)
{
  return Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm, pLengthValue, lengthSizeInBits);
}

asn1SccSint milbus_encode (asn1SccSint
val)
{
  return
  val ==
  32 ? 0:
  val;
}

asn1SccSint milbus_decode (asn1SccSint
val)
{
  return
  val ==
  0 ? 32:
  val;
}


flag Acn_Dec_Int_PositiveInteger_ConstSizeUInt8 (pBitStrm: BitStream, uint8_t * pIntVal, encodedSizeInBits: Int) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm, & v, encodedSizeInBits);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSizeUInt16 (pBitStrm: BitStream, uint16_t * pIntVal, encodedSizeInBits: Int) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm, & v, encodedSizeInBits);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSizeUInt32 (pBitStrm: BitStream, uint32_t * pIntVal, encodedSizeInBits: Int) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm, & v, encodedSizeInBits);
  * pIntVal = (uint32_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_8UInt8 (pBitStrm: BitStream, uint8_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_8(pBitStrm, & v);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16UInt16 (pBitStrm: BitStream, uint16_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16(pBitStrm, & v);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16UInt8 (pBitStrm: BitStream, uint8_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16(pBitStrm, & v);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt32 (pBitStrm: BitStream, uint32_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm, & v);
  * pIntVal = (uint32_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt16 (pBitStrm: BitStream, uint16_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm, & v);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt8 (pBitStrm: BitStream, uint8_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm, & v);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt32 (pBitStrm: BitStream, uint32_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64(pBitStrm, & v);
  * pIntVal = (uint32_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt16 (pBitStrm: BitStream, uint16_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64(pBitStrm, & v);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt8 (pBitStrm: BitStream, uint8_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64(pBitStrm, & v);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16UInt16 (pBitStrm: BitStream, uint16_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16(pBitStrm, & v);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16UInt8 (pBitStrm: BitStream, uint8_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16(pBitStrm, & v);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt32 (pBitStrm: BitStream, uint32_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm, & v);
  * pIntVal = (uint32_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt16 (pBitStrm: BitStream, uint16_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm, & v);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt8 (pBitStrm: BitStream, uint8_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm, & v);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt32 (pBitStrm: BitStream, uint32_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm, & v);
  * pIntVal = (uint32_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt16 (pBitStrm: BitStream, uint16_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm, & v);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt8 (pBitStrm: BitStream, uint8_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm, & v);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt8 (pBitStrm: BitStream, uint8_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt16 (pBitStrm: BitStream, uint16_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt32 (pBitStrm: BitStream, uint32_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (uint32_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSizeInt8 (pBitStrm: BitStream, int8_t * pIntVal, encodedSizeInBits: Int) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize(pBitStrm, & v, encodedSizeInBits);
  * pIntVal = (int8_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSizeInt16 (pBitStrm: BitStream, int16_t * pIntVal, encodedSizeInBits: Int) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize(pBitStrm, & v, encodedSizeInBits);
  * pIntVal = (int16_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSizeInt32 (pBitStrm: BitStream, int32_t * pIntVal, encodedSizeInBits: Int) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize(pBitStrm, & v, encodedSizeInBits);
  * pIntVal = (int32_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_8Int8 (pBitStrm: BitStream, int8_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_8(pBitStrm, & v);
  * pIntVal = (int8_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16Int16 (pBitStrm: BitStream, int16_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16(pBitStrm, & v);
  * pIntVal = (int16_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16Int8 (pBitStrm: BitStream, int8_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16(pBitStrm, & v);
  * pIntVal = (int8_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int32 (pBitStrm: BitStream, int32_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32(pBitStrm, & v);
  * pIntVal = (int32_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int16 (pBitStrm: BitStream, int16_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32(pBitStrm, & v);
  * pIntVal = (int16_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int8 (pBitStrm: BitStream, int8_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32(pBitStrm, & v);
  * pIntVal = (int8_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int32 (pBitStrm: BitStream, int32_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64(pBitStrm, & v);
  * pIntVal = (int32_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int16 (pBitStrm: BitStream, int16_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64(pBitStrm, & v);
  * pIntVal = (int16_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int8 (pBitStrm: BitStream, int8_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64(pBitStrm, & v);
  * pIntVal = (int8_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16Int16 (pBitStrm: BitStream, int16_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16(pBitStrm, & v);
  * pIntVal = (int16_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16Int8 (pBitStrm: BitStream, int8_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16(pBitStrm, & v);
  * pIntVal = (int8_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int32 (pBitStrm: BitStream, int32_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32(pBitStrm, & v);
  * pIntVal = (int32_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int16 (pBitStrm: BitStream, int16_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32(pBitStrm, & v);
  * pIntVal = (int16_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int8 (pBitStrm: BitStream, int8_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32(pBitStrm, & v);
  * pIntVal = (int8_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int32 (pBitStrm: BitStream, int32_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64(pBitStrm, & v);
  * pIntVal = (int32_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int16 (pBitStrm: BitStream, int16_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64(pBitStrm, & v);
  * pIntVal = (int16_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int8 (pBitStrm: BitStream, int8_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64(pBitStrm, & v);
  * pIntVal = (int8_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_VarSize_LengthEmbeddedInt8 (pBitStrm: BitStream, int8_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (int8_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_VarSize_LengthEmbeddedInt16 (pBitStrm: BitStream, int16_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (int16_t) v;
  return ret;
}


flag Acn_Dec_Int_TwosComplement_VarSize_LengthEmbeddedInt32 (pBitStrm: BitStream, int32_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (int32_t) v;
  return ret;
}


flag Acn_Dec_Int_BCD_ConstSizeUInt8 (pBitStrm: BitStream, uint8_t * pIntVal, int encodedSizeInNibbles) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_BCD_ConstSize(pBitStrm, & v, encodedSizeInNibbles);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_Int_BCD_ConstSizeUInt16 (pBitStrm: BitStream, uint16_t * pIntVal, int encodedSizeInNibbles) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_BCD_ConstSize(pBitStrm, & v, encodedSizeInNibbles);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_Int_BCD_ConstSizeUInt32 (pBitStrm: BitStream, uint32_t * pIntVal, int encodedSizeInNibbles) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_BCD_ConstSize(pBitStrm, & v, encodedSizeInNibbles);
  * pIntVal = (uint32_t) v;
  return ret;
}


flag Acn_Dec_Int_BCD_VarSize_LengthEmbeddedUInt8 (pBitStrm: BitStream, uint8_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_BCD_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_Int_BCD_VarSize_LengthEmbeddedUInt16 (pBitStrm: BitStream, uint16_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_BCD_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_Int_BCD_VarSize_LengthEmbeddedUInt32 (pBitStrm: BitStream, uint32_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_BCD_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (uint32_t) v;
  return ret;
}


flag Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt8 (pBitStrm: BitStream, uint8_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_BCD_VarSize_NullTerminated(pBitStrm, & v);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt16 (pBitStrm: BitStream, uint16_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_BCD_VarSize_NullTerminated(pBitStrm, & v);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt32 (pBitStrm: BitStream, uint32_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_Int_BCD_VarSize_NullTerminated(pBitStrm, & v);
  * pIntVal = (uint32_t) v;
  return ret;
}


flag Acn_Dec_SInt_ASCII_ConstSizeInt8 (pBitStrm: BitStream, int8_t * pIntVal, int encodedSizeInBytes) {
  asn1SccSint v;
  flag ret = Acn_Dec_SInt_ASCII_ConstSize(pBitStrm, & v, encodedSizeInBytes);
  * pIntVal = (int8_t) v;
  return ret;
}


flag Acn_Dec_SInt_ASCII_ConstSizeInt16 (pBitStrm: BitStream, int16_t * pIntVal, int encodedSizeInBytes) {
  asn1SccSint v;
  flag ret = Acn_Dec_SInt_ASCII_ConstSize(pBitStrm, & v, encodedSizeInBytes);
  * pIntVal = (int16_t) v;
  return ret;
}


flag Acn_Dec_SInt_ASCII_ConstSizeInt32 (pBitStrm: BitStream, int32_t * pIntVal, int encodedSizeInBytes) {
  asn1SccSint v;
  flag ret = Acn_Dec_SInt_ASCII_ConstSize(pBitStrm, & v, encodedSizeInBytes);
  * pIntVal = (int32_t) v;
  return ret;
}


flag Acn_Dec_SInt_ASCII_VarSize_LengthEmbeddedInt8 (pBitStrm: BitStream, int8_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_SInt_ASCII_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (int8_t) v;
  return ret;
}


flag Acn_Dec_SInt_ASCII_VarSize_LengthEmbeddedInt16 (pBitStrm: BitStream, int16_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_SInt_ASCII_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (int16_t) v;
  return ret;
}


flag Acn_Dec_SInt_ASCII_VarSize_LengthEmbeddedInt32 (pBitStrm: BitStream, int32_t * pIntVal) {
  asn1SccSint v;
  flag ret = Acn_Dec_SInt_ASCII_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (int32_t) v;
  return ret;
}


flag Acn_Dec_SInt_ASCII_VarSize_NullTerminatedInt8 (pBitStrm: BitStream, int8_t * pIntVal, const byte null_characters[], size_t null_characters_size) {
  asn1SccSint v;
  flag ret = Acn_Dec_SInt_ASCII_VarSize_NullTerminated(pBitStrm, & v, null_characters, null_characters_size);
  * pIntVal = (int8_t) v;
  return ret;
}


flag Acn_Dec_SInt_ASCII_VarSize_NullTerminatedInt16 (pBitStrm: BitStream, int16_t * pIntVal, const byte null_characters[], size_t null_characters_size) {
  asn1SccSint v;
  flag ret = Acn_Dec_SInt_ASCII_VarSize_NullTerminated(pBitStrm, & v, null_characters, null_characters_size);
  * pIntVal = (int16_t) v;
  return ret;
}


flag Acn_Dec_SInt_ASCII_VarSize_NullTerminatedInt32 (pBitStrm: BitStream, int32_t * pIntVal, const byte null_characters[], size_t null_characters_size) {
  asn1SccSint v;
  flag ret = Acn_Dec_SInt_ASCII_VarSize_NullTerminated(pBitStrm, & v, null_characters, null_characters_size);
  * pIntVal = (int32_t) v;
  return ret;
}


flag Acn_Dec_UInt_ASCII_ConstSizeUInt8 (pBitStrm: BitStream, uint8_t * pIntVal, int encodedSizeInBytes) {
  asn1SccUint v;
  flag ret = Acn_Dec_UInt_ASCII_ConstSize(pBitStrm, & v, encodedSizeInBytes);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_UInt_ASCII_ConstSizeUInt16 (pBitStrm: BitStream, uint16_t * pIntVal, int encodedSizeInBytes) {
  asn1SccUint v;
  flag ret = Acn_Dec_UInt_ASCII_ConstSize(pBitStrm, & v, encodedSizeInBytes);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_UInt_ASCII_ConstSizeUInt32 (pBitStrm: BitStream, uint32_t * pIntVal, int encodedSizeInBytes) {
  asn1SccUint v;
  flag ret = Acn_Dec_UInt_ASCII_ConstSize(pBitStrm, & v, encodedSizeInBytes);
  * pIntVal = (uint32_t) v;
  return ret;
}


flag Acn_Dec_UInt_ASCII_VarSize_LengthEmbeddedUInt8 (pBitStrm: BitStream, uint8_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_UInt_ASCII_VarSize_LengthEmbeddedUInt16 (pBitStrm: BitStream, uint16_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_UInt_ASCII_VarSize_LengthEmbeddedUInt32 (pBitStrm: BitStream, uint32_t * pIntVal) {
  asn1SccUint v;
  flag ret = Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded(pBitStrm, & v);
  * pIntVal = (uint32_t) v;
  return ret;
}


flag Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt8 (pBitStrm: BitStream, uint8_t * pIntVal, const byte null_characters[], size_t null_characters_size) {
  asn1SccUint v;
  flag ret = Acn_Dec_UInt_ASCII_VarSize_NullTerminated(pBitStrm, & v, null_characters, null_characters_size);
  * pIntVal = (uint8_t) v;
  return ret;
}


flag Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt16 (pBitStrm: BitStream, uint16_t * pIntVal, const byte null_characters[], size_t null_characters_size) {
  asn1SccUint v;
  flag ret = Acn_Dec_UInt_ASCII_VarSize_NullTerminated(pBitStrm, & v, null_characters, null_characters_size);
  * pIntVal = (uint16_t) v;
  return ret;
}


flag Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt32 (pBitStrm: BitStream, uint32_t * pIntVal, const byte null_characters[], size_t null_characters_size) {
  asn1SccUint v;
  flag ret = Acn_Dec_UInt_ASCII_VarSize_NullTerminated(pBitStrm, & v, null_characters, null_characters_size);
  * pIntVal = (uint32_t) v;
  return ret;
}
*/

