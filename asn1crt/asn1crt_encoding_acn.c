#include <string.h>
#include <assert.h>

#include "asn1crt_encoding_acn.h"

static byte masks[] = { 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 };

static flag RequiresReverse(void)
{
	short int word = 0x0001;
	char *b = (char *)&word;
	return b[0] == 1;
}



void Acn_AlignToNextByte(BitStream* pBitStrm, flag bEncode)
{
	if (pBitStrm->currentBit != 0)
	{
		pBitStrm->currentBit = 0;
		pBitStrm->currentByte++;
		if (bEncode)
			bitstrean_push_data_if_required(pBitStrm);
		else
            bitstrean_fetch_data_if_required(pBitStrm);
		CHECK_BIT_STREAM(pBitStrm);
	}
}

void Acn_AlignToNextWord(BitStream* pBitStrm, flag bEncode)
{
	Acn_AlignToNextByte(pBitStrm, bEncode);

	pBitStrm->currentByte += pBitStrm->currentByte % 2;
	if (bEncode)
		bitstrean_push_data_if_required(pBitStrm);
	else
        bitstrean_fetch_data_if_required(pBitStrm);

	CHECK_BIT_STREAM(pBitStrm);
}

void Acn_AlignToNextDWord(BitStream* pBitStrm, flag bEncode)
{
	Acn_AlignToNextByte(pBitStrm, bEncode);

	//pBitStrm->currentByte += pBitStrm->currentByte % 4;
	int totalBytes = pBitStrm->currentByte % 4;
	if (pBitStrm->currentByte + totalBytes < pBitStrm->count) {
		pBitStrm->currentByte += totalBytes;
	}
	else {
		int extraBytes = pBitStrm->currentByte + totalBytes - pBitStrm->count;
		pBitStrm->currentByte = pBitStrm->count;
		if (bEncode)
			bitstrean_push_data_if_required(pBitStrm);
		else
            bitstrean_fetch_data_if_required(pBitStrm);
		pBitStrm->currentByte = extraBytes;
	}


	CHECK_BIT_STREAM(pBitStrm);
}

/*ACN Integer functions*/
void Acn_Enc_Int_PositiveInteger_ConstSize(BitStream* pBitStrm, asn1SccUint intVal, int encodedSizeInBits)
{
	int nBits = 0;
	if (encodedSizeInBits == 0)
		return;
	/* Get number of bits*/
	nBits = GetNumberOfBitsForNonNegativeInteger(intVal);
	/* put required zeros*/
	BitStream_AppendNBitZero(pBitStrm, encodedSizeInBits - nBits);
	/*Encode number */
	BitStream_EncodeNonNegativeInteger(pBitStrm, intVal);

	CHECK_BIT_STREAM(pBitStrm);
}
void Acn_Enc_Int_PositiveInteger_ConstSize_8(BitStream* pBitStrm, asn1SccUint intVal)
{
	BitStream_AppendByte0(pBitStrm, (byte)intVal);
	CHECK_BIT_STREAM(pBitStrm);
}

static void Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_B(BitStream* pBitStrm,
	asn1SccUint intVal,
	int size)
{
	int i = 0;
	asn1SccUint tmp = intVal;
	asn1SccUint mask = 0xFF;
	mask <<= (size - 1) * 8;

	for (i = 0; i<size; i++) {
		byte ByteToEncode = (byte)((tmp & mask) >> ((size - i - 1) * 8));
		BitStream_AppendByte0(pBitStrm, ByteToEncode);
		mask >>= 8;
	}
	CHECK_BIT_STREAM(pBitStrm);
}

void Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16(BitStream* pBitStrm, asn1SccUint intVal)
{
	Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_B(pBitStrm, intVal, 2);
}

void Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32(BitStream* pBitStrm, asn1SccUint intVal)
{
	Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_B(pBitStrm, intVal, 4);
}

void Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64(BitStream* pBitStrm, asn1SccUint intVal)
{
    // Avoid dead code warnings by conditionally compiling this part.
#if WORD_SIZE!=8
    int i;
    for (i = 0; i < 8 - WORD_SIZE; i++) {
        BitStream_AppendByte0(pBitStrm, 0x0);
    }
#endif
	Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_B(pBitStrm, intVal, WORD_SIZE);
}

static void Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(BitStream* pBitStrm,
	asn1SccUint intVal,
	int size)
{
	int i = 0;
	asn1SccUint tmp = intVal;

	for (i = 0; i<size; i++) {
		byte ByteToEncode = (byte)tmp;
		BitStream_AppendByte0(pBitStrm, ByteToEncode);
		tmp >>= 8;
	}
	CHECK_BIT_STREAM(pBitStrm);
}

void Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16(BitStream* pBitStrm, asn1SccUint intVal)
{
	Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, intVal, 2);
}

void Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32(BitStream* pBitStrm, asn1SccUint intVal)
{
	Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, intVal, 4);
}

void Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64(BitStream* pBitStrm, asn1SccUint intVal)
{
    Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, intVal, WORD_SIZE);
    // Avoid dead code warnings by conditionally compiling this part.
#if WORD_SIZE!=8
    int i;
    for (i = 0; i < 8 - WORD_SIZE; i++) {
        BitStream_AppendByte0(pBitStrm, 0x0);
    }
#endif
}


flag Acn_Dec_Int_PositiveInteger_ConstSize(BitStream* pBitStrm, asn1SccUint* pIntVal, int encodedSizeInBits)
{
	asn1SccUint tmp = 0;
	if (BitStream_DecodeNonNegativeInteger(pBitStrm, &tmp, encodedSizeInBits))
	{
		*pIntVal = tmp;
		return TRUE;
	}
	return FALSE;

}


flag Acn_Dec_Int_PositiveInteger_ConstSize_8(BitStream* pBitStrm, asn1SccUint* pIntVal)
{
	byte v = 0;
	if (!BitStream_ReadByte(pBitStrm, &v))
		return FALSE;
	*pIntVal = v;
	return TRUE;
}

static flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_N(BitStream* pBitStrm,
	asn1SccUint* pIntVal,
	int SizeInBytes)
{
	int i;
	asn1SccUint ret = 0;

	*pIntVal = 0;

	for (i = 0; i<SizeInBytes; i++) {
		byte b = 0;
		if (!BitStream_ReadByte(pBitStrm, &b))
			return FALSE;
		ret <<= 8;
		ret |= b;
	}
	*pIntVal = ret;
	return TRUE;
}

flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16(BitStream* pBitStrm, asn1SccUint* pIntVal)
{
	return Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_N(pBitStrm, pIntVal, 2);
}

flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32(BitStream* pBitStrm, asn1SccUint* pIntVal)
{
	return Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_N(pBitStrm, pIntVal, 4);
}

flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64(BitStream* pBitStrm, asn1SccUint* pIntVal)
{
    pBitStrm->currentByte += (8 - WORD_SIZE);

	return Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_N(pBitStrm, pIntVal, WORD_SIZE);
}

static flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(BitStream* pBitStrm,
	asn1SccUint* pIntVal,
	int SizeInBytes)
{
	int i;
	asn1SccUint ret = 0;
	asn1SccUint tmp = 0;

	*pIntVal = 0;

	for (i = 0; i<SizeInBytes; i++) {
		byte b = 0;
		if (!BitStream_ReadByte(pBitStrm, &b))
			return FALSE;
		tmp = b;
		tmp <<= i * 8;
		ret |= tmp;
	}
	*pIntVal = ret;
	return TRUE;

}

flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16(BitStream* pBitStrm, asn1SccUint* pIntVal)
{
	return Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, pIntVal, 2);
}

flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32(BitStream* pBitStrm, asn1SccUint* pIntVal)
{
	return Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, pIntVal, 4);
}

flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64(BitStream* pBitStrm, asn1SccUint* pIntVal)
{
    flag ret;
	ret = Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N(pBitStrm, pIntVal, WORD_SIZE);
    pBitStrm->currentByte += (8 - WORD_SIZE);
    return ret;
}




static void Encode_UnsignedInteger(BitStream* pBitStrm, asn1SccUint val, byte nBytes)
{
#if WORD_SIZE==8
#define MAX_BYTE_MASK 0xFF00000000000000LL
#else
#define MAX_BYTE_MASK 0xFF000000
#endif
	int i = 0;
	assert(nBytes <= 8);
	val <<= (WORD_SIZE * 8U - nBytes * 8U);
	for (i = 0; i<nBytes; i++) {
		byte ByteToEncode = (byte)((val & MAX_BYTE_MASK) >> ((WORD_SIZE - 1) * 8));
		BitStream_AppendByte0(pBitStrm, ByteToEncode);
		val <<= 8;
	}
}


void Acn_Enc_Int_PositiveInteger_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint intVal)
{
	asn1SccUint val = intVal;
	byte nBytes = (byte)GetLengthInBytesOfUInt(val);


	/* encode length */
	BitStream_AppendByte0(pBitStrm, nBytes);
	/* Encode integer data*/
	Encode_UnsignedInteger(pBitStrm, val, nBytes);


	CHECK_BIT_STREAM(pBitStrm);
}

flag Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint* pIntVal)
{
	byte nBytes;
	int i;
	asn1SccUint v = 0;
	if (!BitStream_ReadByte(pBitStrm, &nBytes))
		return FALSE;
	for (i = 0; i<nBytes; i++) {
		byte b = 0;
		if (!BitStream_ReadByte(pBitStrm, &b))
			return FALSE;
		v = (v << 8) | b;
	}
	*pIntVal = v;
	return TRUE;
}


void Acn_Enc_Int_TwosComplement_ConstSize(BitStream* pBitStrm, asn1SccSint intVal, int encodedSizeInBits)
{
	if (intVal >= 0) {
		BitStream_AppendNBitZero(pBitStrm, encodedSizeInBits - GetNumberOfBitsForNonNegativeInteger((asn1SccUint)intVal));
		BitStream_EncodeNonNegativeInteger(pBitStrm, (asn1SccUint)intVal);
	}
	else {
		BitStream_AppendNBitOne(pBitStrm, encodedSizeInBits - GetNumberOfBitsForNonNegativeInteger((asn1SccUint)(-intVal - 1)));
		BitStream_EncodeNonNegativeIntegerNeg(pBitStrm, (asn1SccUint)(-intVal - 1), 1);
	}
	CHECK_BIT_STREAM(pBitStrm);

}




void Acn_Enc_Int_TwosComplement_ConstSize_8(BitStream* pBitStrm, asn1SccSint intVal)
{
	Acn_Enc_Int_PositiveInteger_ConstSize_8(pBitStrm, int2uint(intVal));
}

void Acn_Enc_Int_TwosComplement_ConstSize_big_endian_16(BitStream* pBitStrm, asn1SccSint intVal)
{
	Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16(pBitStrm, int2uint(intVal));
}

void Acn_Enc_Int_TwosComplement_ConstSize_big_endian_32(BitStream* pBitStrm, asn1SccSint intVal)
{
	Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm, int2uint(intVal));
}

void Acn_Enc_Int_TwosComplement_ConstSize_big_endian_64(BitStream* pBitStrm, asn1SccSint intVal)
{
    Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64(pBitStrm, int2uint(intVal));
}

void Acn_Enc_Int_TwosComplement_ConstSize_little_endian_16(BitStream* pBitStrm, asn1SccSint intVal)
{
	Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16(pBitStrm, int2uint(intVal));
}

void Acn_Enc_Int_TwosComplement_ConstSize_little_endian_32(BitStream* pBitStrm, asn1SccSint intVal)
{
	Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm, int2uint(intVal));
}

void Acn_Enc_Int_TwosComplement_ConstSize_little_endian_64(BitStream* pBitStrm, asn1SccSint intVal)
{
	Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm, int2uint(intVal));
}




flag Acn_Dec_Int_TwosComplement_ConstSize(BitStream* pBitStrm, asn1SccSint* pIntVal, int encodedSizeInBits)
{
	int i;
	flag valIsNegative = FALSE;
	int nBytes = encodedSizeInBits / 8;
	int rstBits = encodedSizeInBits % 8;
	byte b = 0;

	*pIntVal = 0;


	for (i = 0; i<nBytes; i++) {
		if (!BitStream_ReadByte(pBitStrm, &b))
			return FALSE;
		if (i == 0) {
			valIsNegative = b>0x7F;
			if (valIsNegative)
				*pIntVal = -1;
		}
		*pIntVal = (*pIntVal << 8) | b;
	}

	if (rstBits>0)
	{
		if (!BitStream_ReadPartialByte(pBitStrm, &b, (byte)rstBits))
			return FALSE;
		if (nBytes == 0)
		{
			valIsNegative = b & masks[8 - rstBits];
			if (valIsNegative)
				*pIntVal = -1;
		}
		*pIntVal = (*pIntVal << rstBits) | b;
	}

	return TRUE;
}

flag Acn_Dec_Int_TwosComplement_ConstSize_8(BitStream* pBitStrm, asn1SccSint* pIntVal)
{
	asn1SccUint tmp = 0;
	if (!Acn_Dec_Int_PositiveInteger_ConstSize_8(pBitStrm, &tmp))
		return FALSE;
	*pIntVal = uint2int(tmp, 1);
	return TRUE;
}

flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16(BitStream* pBitStrm, asn1SccSint* pIntVal)
{
	asn1SccUint tmp = 0;
	if (!Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16(pBitStrm, &tmp))
		return FALSE;
	*pIntVal = uint2int(tmp, 2);
	return TRUE;
}

flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32(BitStream* pBitStrm, asn1SccSint* pIntVal)
{
	asn1SccUint tmp = 0;
	if (!Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm, &tmp))
		return FALSE;
	*pIntVal = uint2int(tmp, 4);
	return TRUE;
}

flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64(BitStream* pBitStrm, asn1SccSint* pIntVal)
{
	asn1SccUint tmp = 0;
	if (!Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64(pBitStrm, &tmp))
		return FALSE;
	*pIntVal = uint2int(tmp, WORD_SIZE);
	return TRUE;
}

flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16(BitStream* pBitStrm, asn1SccSint* pIntVal)
{
	asn1SccUint tmp = 0;
	if (!Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16(pBitStrm, &tmp))
		return FALSE;
	*pIntVal = uint2int(tmp, 2);
	return TRUE;
}

flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32(BitStream* pBitStrm, asn1SccSint* pIntVal)
{
	asn1SccUint tmp = 0;
	if (!Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32(pBitStrm, &tmp))
		return FALSE;
	*pIntVal = uint2int(tmp, 4);
	return TRUE;
}

flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64(BitStream* pBitStrm, asn1SccSint* pIntVal)
{
	asn1SccUint tmp = 0;
	if (!Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64(pBitStrm, &tmp))
		return FALSE;
	*pIntVal = uint2int(tmp, WORD_SIZE);
	return TRUE;
}




static asn1SccUint To_UInt(asn1SccSint intVal) {
	asn1SccUint ret = 0;
	if (intVal <0) {
		ret = (asn1SccUint)(-intVal - 1);
		ret = ~ret;
	}
	else {
		ret = (asn1SccUint)intVal;
	}
	return ret;
}


void Acn_Enc_Int_TwosComplement_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccSint intVal)
{

	byte nBytes = (byte)GetLengthInBytesOfSInt(intVal);

	/* encode length */
	BitStream_AppendByte0(pBitStrm, nBytes);
	/* Encode integer data*/
	Encode_UnsignedInteger(pBitStrm, To_UInt(intVal), nBytes);


	CHECK_BIT_STREAM(pBitStrm);

}


flag Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccSint* pIntVal)
{

#if WORD_SIZE==8
#define MAX_INT 0xFFFFFFFFFFFFFFFFULL
#else
#define MAX_INT 0xFFFFFFFF
#endif

	byte nBytes;
	int i;
	asn1SccUint v = 0;
	flag isNegative = 0;
	if (!BitStream_ReadByte(pBitStrm, &nBytes))
		return FALSE;
	for (i = 0; i<nBytes; i++) {
		byte b = 0;
		if (!BitStream_ReadByte(pBitStrm, &b))
			return FALSE;
		if (i == 0 && (b & 0x80)>0) {
			v = MAX_INT;
			isNegative = 1;
		}

		v = (v << 8) | b;
	}
	if (isNegative) {
		*pIntVal = -((asn1SccSint)(~v)) - 1;
	}
	else {
		*pIntVal = (asn1SccSint)v;
	}
	return TRUE;

}


//return values is in nibbles
static int Acn_Get_Int_Size_BCD(asn1SccUint intVal)
{
	int ret = 0;
	while (intVal>0)
	{
		intVal /= 10;
		ret++;
	}
	return ret;
}




void Acn_Enc_Int_BCD_ConstSize(BitStream* pBitStrm, asn1SccUint intVal, int encodedSizeInNibbles)
{
	int i = 0;
	int totalNibbles = 0;
	char tmp[100];

	memset(tmp, 0x0, sizeof(tmp));

	assert(100 >= encodedSizeInNibbles);

	while (intVal>0) {
		tmp[totalNibbles++] = (char)(intVal % 10);
		intVal /= 10;
	}
	assert(encodedSizeInNibbles >= totalNibbles);

	for (i = encodedSizeInNibbles - 1; i >= 0; i--) {
		BitStream_AppendPartialByte(pBitStrm, (byte)tmp[i], 4, 0);
	}

	CHECK_BIT_STREAM(pBitStrm);
}



flag Acn_Dec_Int_BCD_ConstSize(BitStream* pBitStrm, asn1SccUint* pIntVal, int encodedSizeInNibbles)
{
	byte digit;
	asn1SccUint ret = 0;


	while (encodedSizeInNibbles>0)
	{
		if (!BitStream_ReadPartialByte(pBitStrm, &digit, 4))
			return FALSE;

		ret *= 10;
		ret += digit;

		encodedSizeInNibbles--;
	}
	*pIntVal = ret;

	return TRUE;
}


void Acn_Enc_Int_BCD_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint intVal)
{
	int nNibbles = Acn_Get_Int_Size_BCD(intVal);
	/* encode length */
	BitStream_AppendByte0(pBitStrm, (byte)nNibbles);


	/* Encode Number */
	Acn_Enc_Int_BCD_ConstSize(pBitStrm, intVal, nNibbles);

	CHECK_BIT_STREAM(pBitStrm);
}

flag Acn_Dec_Int_BCD_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint* pIntVal)
{
	byte nNibbles = 0;
	if (BitStream_ReadByte(pBitStrm, &nNibbles))
		return Acn_Dec_Int_BCD_ConstSize(pBitStrm, pIntVal, nNibbles);

	return FALSE;

}



//encoding puts an 'F' at the end
void Acn_Enc_Int_BCD_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccUint intVal)
{

	int nNibbles = Acn_Get_Int_Size_BCD(intVal);

	/* Encode Number */
	Acn_Enc_Int_BCD_ConstSize(pBitStrm, intVal, nNibbles);

	BitStream_AppendPartialByte(pBitStrm, 0xF, 4, 0);

	CHECK_BIT_STREAM(pBitStrm);
}

flag Acn_Dec_Int_BCD_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccUint* pIntVal)
{
	byte digit;
	asn1SccUint ret = 0;

	for (;;)
	{
		if (!BitStream_ReadPartialByte(pBitStrm, &digit, 4))
			return FALSE;
		if (digit>9)
			break;

		ret *= 10;
		ret += digit;

	}
	*pIntVal = ret;

	return TRUE;
}


void Acn_Enc_UInt_ASCII_ConstSize(BitStream* pBitStrm, asn1SccUint intVal, int encodedSizeInBytes)
{
	int i = 0;
	int totalNibbles = 0;
	char tmp[100];

	memset(tmp, 0x0, sizeof(tmp));

	assert(100 >= encodedSizeInBytes);

	while (intVal>0) {
		tmp[totalNibbles++] = (char)(intVal % 10);
		intVal /= 10;
	}
	assert(encodedSizeInBytes >= totalNibbles);

	for (i = encodedSizeInBytes - 1; i >= 0; i--) {
		BitStream_AppendByte0(pBitStrm, (byte)(tmp[i] + '0'));
	}

	CHECK_BIT_STREAM(pBitStrm);
}


void Acn_Enc_SInt_ASCII_ConstSize(BitStream* pBitStrm, asn1SccSint intVal, int encodedSizeInBytes)
{
	asn1SccUint absIntVal = intVal >= 0 ? (asn1SccUint)intVal : (asn1SccUint)(-intVal);

	/* encode sign */
	BitStream_AppendByte0(pBitStrm, intVal >= 0 ? '+' : '-');
	encodedSizeInBytes--;



	Acn_Enc_UInt_ASCII_ConstSize(pBitStrm, absIntVal, encodedSizeInBytes);

}

flag Acn_Dec_UInt_ASCII_ConstSize(BitStream* pBitStrm, asn1SccUint* pIntVal, int encodedSizeInBytes)
{
	byte digit;
	asn1SccUint ret = 0;

	while (encodedSizeInBytes>0)
	{
		if (!BitStream_ReadByte(pBitStrm, &digit))
			return FALSE;
		ASSERT_OR_RETURN_FALSE(digit >= '0' && digit <= '9');
		digit = (byte)((int)digit - '0');

		ret *= 10;
		ret += digit;

		encodedSizeInBytes--;
	}
	*pIntVal = ret;

	return TRUE;
}

flag Acn_Dec_SInt_ASCII_ConstSize(BitStream* pBitStrm, asn1SccSint* pIntVal, int encodedSizeInBytes)
{
	byte digit;
	asn1SccUint ret = 0;
	int sign = 1;

	if (!BitStream_ReadByte(pBitStrm, &digit))
		return FALSE;
	if (digit == '+')
		sign = 1;
	else if (digit == '-')
		sign = -1;
	else {
		ASSERT_OR_RETURN_FALSE(0);
	}
	encodedSizeInBytes--;


	if (!Acn_Dec_UInt_ASCII_ConstSize(pBitStrm, &ret, encodedSizeInBytes)) {
		return false;
	}

	*pIntVal = (asn1SccSint)ret;

	*pIntVal = sign*(*pIntVal);
	return TRUE;

}


void getIntegerDigits(asn1SccUint intVal, byte digitsArray100[], byte* totalDigits);

void getIntegerDigits(asn1SccUint intVal, byte digitsArray100[], byte* totalDigits) {
	int i = 0;
	*totalDigits = 0;
	byte reversedDigitsArray[100];
	memset(reversedDigitsArray, 0x0, 100);
	memset(digitsArray100, 0x0, 100);
	if (intVal > 0) {
		while (intVal > 0 && *totalDigits < 100) {
			reversedDigitsArray[*totalDigits] = '0' + (byte)(intVal % 10);
			(*totalDigits)++;
			intVal /= 10;
		}
		for (i = *totalDigits - 1; i >= 0; i--) {
			digitsArray100[(*totalDigits - 1) - i] = reversedDigitsArray[i];
		}
	}
	else {
		digitsArray100[0] = '0';
		*totalDigits = 1;
	}
}


void Acn_Enc_SInt_ASCII_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccSint intVal)
{
	byte digitsArray100[100];
	int i = 0;
	byte nChars;
	asn1SccUint absIntVal = intVal >= 0 ? (asn1SccUint)intVal : (asn1SccUint)(-intVal);
	getIntegerDigits(absIntVal, digitsArray100, &nChars);

	/* encode length, plus 1 for sign */
	BitStream_AppendByte0(pBitStrm, nChars + 1);

	/* encode sign */
	BitStream_AppendByte0(pBitStrm, intVal >= 0 ? '+' : '-');

	/* encode digits */
	while (digitsArray100[i] != 0x0 && i<100) {
		BitStream_AppendByte0(pBitStrm, digitsArray100[i]);
		i++;
	}

	CHECK_BIT_STREAM(pBitStrm);

}

void Acn_Enc_UInt_ASCII_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint intVal)
{
	byte digitsArray100[100];
	int i = 0;
	byte nChars;
	getIntegerDigits(intVal, digitsArray100, &nChars);

	/* encode length */
	BitStream_AppendByte0(pBitStrm, nChars);
	/* encode digits */
	while (digitsArray100[i] != 0x0 && i<100) {
		BitStream_AppendByte0(pBitStrm, digitsArray100[i]);
		i++;
	}

	CHECK_BIT_STREAM(pBitStrm);

}


flag Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint* pIntVal)
{
	byte nChars = 0;
	if (BitStream_ReadByte(pBitStrm, &nChars))
		return Acn_Dec_UInt_ASCII_ConstSize(pBitStrm, pIntVal, nChars);

	return FALSE;
}

flag Acn_Dec_SInt_ASCII_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccSint* pIntVal)
{
	byte nChars = 0;
	if (BitStream_ReadByte(pBitStrm, &nChars))
		return Acn_Dec_SInt_ASCII_ConstSize(pBitStrm, pIntVal, nChars);

	return FALSE;
}



void Acn_Enc_UInt_ASCII_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccUint intVal, const byte null_characters[], size_t null_characters_size)
{
	byte digitsArray100[100];
	byte nChars;
	size_t i = 0;
	getIntegerDigits(intVal, digitsArray100, &nChars);
	while (digitsArray100[i] != 0x0 && i<100) {
		BitStream_AppendByte0(pBitStrm, digitsArray100[i]);
		i++;
	}
	for (i = 0; i<null_characters_size; i++)
		BitStream_AppendByte0(pBitStrm, null_characters[i]);
	CHECK_BIT_STREAM(pBitStrm);
}

void Acn_Enc_SInt_ASCII_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccSint intVal, const byte null_characters[], size_t null_characters_size)
{
	asn1SccUint absValue = intVal >= 0 ? (asn1SccUint)intVal : (asn1SccUint)(-intVal);
	BitStream_AppendByte0(pBitStrm, intVal >= 0 ? '+' : '-');

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

flag Acn_Dec_UInt_ASCII_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccUint* pIntVal, const byte null_characters[], size_t null_characters_size)
{
	byte digit;
	asn1SccUint ret = 0;
	byte tmp[10];
	size_t sz = null_characters_size < 10 ? null_characters_size : 10;
	memset(tmp, 0x0, 10);
	asn1SccSint i = 0;

	//read null_character_size characters into the tmp buffer
	for (int j = 0; j < (int)null_characters_size; j++) {
		if (!BitStream_ReadByte(pBitStrm, &(tmp[j])))
			return FALSE;
	}

	while (memcmp(null_characters, tmp, sz) != 0) {
		digit = tmp[0];
		i++;
		for (int j = 0; j < (int)null_characters_size - 1; j++)
			tmp[j] = tmp[j + 1];
		if (!BitStream_ReadByte(pBitStrm, &(tmp[null_characters_size - 1])))
			return FALSE;

		digit = (byte)((int)digit - '0');

		ret *= 10;
		ret += digit;
	}

	*pIntVal = ret;

	return TRUE;
}


flag Acn_Dec_SInt_ASCII_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccSint* pIntVal, const byte null_characters[], size_t null_characters_size)
{
	byte digit;
	asn1SccUint ret = 0;
	flag isNegative = FALSE;

	if (!BitStream_ReadByte(pBitStrm, &digit))
		return FALSE;
	ASSERT_OR_RETURN_FALSE(digit == '-' || digit == '+');
	if (digit == '-')
		isNegative = TRUE;

	if (!Acn_Dec_UInt_ASCII_VarSize_NullTerminated(pBitStrm, &ret, null_characters, null_characters_size))
		return false;

	*pIntVal = (asn1SccSint)ret;
	if (isNegative)
		*pIntVal = -(*pIntVal);
	return TRUE;
}





/* Boolean Decode */

flag BitStream_ReadBitPattern(BitStream* pBitStrm, const byte* patternToRead, int nBitsToRead, flag* pBoolValue)
{
	int nBytesToRead = nBitsToRead / 8;
	int nRemainingBitsToRead = nBitsToRead % 8;
	byte curByte;
	int i = 0;

	*pBoolValue = TRUE;
	for (i = 0; i<nBytesToRead; i++) {
		if (!BitStream_ReadByte(pBitStrm, &curByte))
			return FALSE;
		if (curByte != patternToRead[i])
			*pBoolValue = FALSE;
	}

	if (nRemainingBitsToRead > 0) {
		if (!BitStream_ReadPartialByte(pBitStrm, &curByte, (byte)nRemainingBitsToRead))
			return FALSE;
		if (curByte != patternToRead[nBytesToRead] >> (8 - nRemainingBitsToRead))
			*pBoolValue = FALSE;
	}

	return TRUE;
}

flag BitStream_ReadBitPattern_ignore_value(BitStream* pBitStrm, int nBitsToRead)
{
	int nBytesToRead = nBitsToRead / 8;
	int nRemainingBitsToRead = nBitsToRead % 8;
	byte curByte;
	int i = 0;

	for (i = 0; i<nBytesToRead; i++) {
		if (!BitStream_ReadByte(pBitStrm, &curByte))
			return FALSE;
	}

	if (nRemainingBitsToRead > 0) {
		if (!BitStream_ReadPartialByte(pBitStrm, &curByte, (byte)nRemainingBitsToRead))
			return FALSE;
	}

	return TRUE;
	
}

/*Real encoding functions*/
typedef union _float_tag
{
	float f;
	byte b[sizeof(float)];
} _float;

typedef union _double_tag
{
	double f;
	byte b[sizeof(double)];
} _double;


#define Acn_enc_real_big_endian(type)       \
    int i;                      \
    _##type dat1;               \
    dat1.f = (type)realValue;   \
    if (!RequiresReverse()) {   \
        for(i=0;i<(int)sizeof(dat1);i++)        \
            BitStream_AppendByte0(pBitStrm,dat1.b[i]);  \
    } else {    \
        for(i=(int)(sizeof(dat1)-1);i>=0;i--)   \
            BitStream_AppendByte0(pBitStrm,dat1.b[i]);  \
    }   \


#define Acn_dec_real_big_endian(type)   \
    int i;                  \
    _##type dat1;           \
    dat1.f=0.0;             \
    if (!RequiresReverse()) {       \
        for(i=0;i<(int)sizeof(dat1);i++) {  \
            if (!BitStream_ReadByte(pBitStrm, &dat1.b[i]))  \
                return FALSE;       \
        }                           \
    } else {                        \
        for(i=(int)(sizeof(dat1)-1);i>=0;i--) {         \
            if (!BitStream_ReadByte(pBitStrm, &dat1.b[i]))      \
                return FALSE;           \
        }       \
    }       \
    *pRealValue = dat1.f;   \
    return TRUE;            \


void Acn_Enc_Real_IEEE754_32_big_endian(BitStream* pBitStrm, asn1Real realValue)
{
	Acn_enc_real_big_endian(float)
}

flag Acn_Dec_Real_IEEE754_32_big_endian(BitStream* pBitStrm, asn1Real* pRealValue)
{
	Acn_dec_real_big_endian(float)
}

void Acn_Enc_Real_IEEE754_64_big_endian(BitStream* pBitStrm, asn1Real realValue)
{
	Acn_enc_real_big_endian(double)
}

flag Acn_Dec_Real_IEEE754_64_big_endian(BitStream* pBitStrm, asn1Real* pRealValue)
{
	Acn_dec_real_big_endian(double)
}




#define Acn_enc_real_little_endian(type)        \
    int i;                      \
    _##type dat1;               \
    dat1.f = (type)realValue;   \
    if (RequiresReverse()) {    \
        for(i=0;i<(int)sizeof(dat1);i++)        \
            BitStream_AppendByte0(pBitStrm,dat1.b[i]);  \
    } else {    \
        for(i=(int)(sizeof(dat1)-1);i>=0;i--)   \
            BitStream_AppendByte0(pBitStrm,dat1.b[i]);  \
    }   \


#define Acn_dec_real_little_endian(type)    \
    int i;                  \
    _##type dat1;           \
    dat1.f=0.0;             \
    if (RequiresReverse()) {        \
        for(i=0;i<(int)sizeof(dat1);i++) {  \
            if (!BitStream_ReadByte(pBitStrm, &dat1.b[i]))  \
                return FALSE;       \
        }                           \
    } else {                        \
        for(i=(int)(sizeof(dat1)-1);i>=0;i--) {         \
            if (!BitStream_ReadByte(pBitStrm, &dat1.b[i]))      \
                return FALSE;           \
        }       \
    }       \
    *pRealValue = dat1.f;   \
    return TRUE;            \


void Acn_Enc_Real_IEEE754_32_little_endian(BitStream* pBitStrm, asn1Real realValue)
{
	Acn_enc_real_little_endian(float)
}

flag Acn_Dec_Real_IEEE754_32_little_endian(BitStream* pBitStrm, asn1Real* pRealValue)
{
	Acn_dec_real_little_endian(float)
}

void Acn_Enc_Real_IEEE754_64_little_endian(BitStream* pBitStrm, asn1Real realValue)
{
	Acn_enc_real_little_endian(double)
}

flag Acn_Dec_Real_IEEE754_64_little_endian(BitStream* pBitStrm, asn1Real* pRealValue)
{
	Acn_dec_real_little_endian(double)
}




/* String functions*/
void Acn_Enc_String_Ascii_FixSize(BitStream* pBitStrm, asn1SccSint max, const char* strVal)
{
	asn1SccSint i = 0;
	while (i<max) {
		BitStream_AppendByte(pBitStrm, strVal[i], FALSE);
		i++;
	}
}

static asn1SccSint Acn_Enc_String_Ascii_private(BitStream* pBitStrm,
	asn1SccSint max,
	const char* strVal)
{
	asn1SccSint i = 0;
	while ((strVal[i] != '\0') && (i<max)) {
		BitStream_AppendByte(pBitStrm, strVal[i], FALSE);
		i++;
	}
	return i;
}

void Acn_Enc_String_Ascii_Null_Teminated(BitStream* pBitStrm, asn1SccSint max, char null_character, const char* strVal)
{
	Acn_Enc_String_Ascii_private(pBitStrm, max, strVal);
	BitStream_AppendByte(pBitStrm, null_character, FALSE);
}

void Acn_Enc_String_Ascii_Null_Teminated_mult(BitStream* pBitStrm, asn1SccSint max, const byte null_character[], size_t null_character_size, const char* strVal) {
	size_t i = 0;
	Acn_Enc_String_Ascii_private(pBitStrm, max, strVal);
	for (i = 0; i<null_character_size; i++) {
		BitStream_AppendByte(pBitStrm, null_character[i], FALSE);
	}
}


void Acn_Enc_String_Ascii_External_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, const char* strVal)
{
	Acn_Enc_String_Ascii_private(pBitStrm, max, strVal);
}

void Acn_Enc_String_Ascii_Internal_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, asn1SccSint min, const char* strVal)
{
	int strLen = strlen(strVal);
	BitStream_EncodeConstraintWholeNumber(pBitStrm, strLen <= max ? strLen : max, min, max);
	Acn_Enc_String_Ascii_private(pBitStrm, max, strVal);
}

void Acn_Enc_String_CharIndex_FixSize(BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, const char* strVal)
{
	asn1SccSint i = 0;
	while (i<max) {
		int charIndex = GetCharIndex(strVal[i], allowedCharSet, charSetSize);
		BitStream_EncodeConstraintWholeNumber(pBitStrm, charIndex, 0, charSetSize - 1);
		i++;
	}
}

static asn1SccSint Acn_Enc_String_CharIndex_private(BitStream* pBitStrm,
	asn1SccSint max,
	byte allowedCharSet[],
	int charSetSize,
	const char* strVal)
{
	asn1SccSint i = 0;
	while ((strVal[i] != '\0') && (i<max)) {
		int charIndex = GetCharIndex(strVal[i], allowedCharSet, charSetSize);
		BitStream_EncodeConstraintWholeNumber(pBitStrm, charIndex, 0, charSetSize - 1);
		i++;
	}
	return i;
}


void Acn_Enc_String_CharIndex_External_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, const char* strVal)
{
	Acn_Enc_String_CharIndex_private(pBitStrm, max, allowedCharSet, charSetSize, strVal);
}

void Acn_Enc_String_CharIndex_Internal_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, asn1SccSint min, const char* strVal)
{
	int strLen = strlen(strVal);
	BitStream_EncodeConstraintWholeNumber(pBitStrm, strLen <= max ? strLen : max, min, max);
	Acn_Enc_String_CharIndex_private(pBitStrm, max, allowedCharSet, charSetSize, strVal);
}


void Acn_Enc_IA5String_CharIndex_External_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, const char* strVal)
{
	static byte allowedCharSet[] = { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,
		0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x10,0x11,0x12,0x13,
		0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,
		0x1E,0x1F,0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,
		0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,0x30,0x31,
		0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,
		0x3C,0x3D,0x3E,0x3F,0x40,0x41,0x42,0x43,0x44,0x45,
		0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
		0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,
		0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,0x60,0x61,0x62,0x63,
		0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,
		0x6E,0x6F,0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,
		0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F };

	Acn_Enc_String_CharIndex_private(pBitStrm, max, allowedCharSet, 128, strVal);
}

void Acn_Enc_IA5String_CharIndex_Internal_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, asn1SccSint min, const char* strVal)
{
	static byte allowedCharSet[] = { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,
		0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x10,0x11,0x12,0x13,
		0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,
		0x1E,0x1F,0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,
		0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,0x30,0x31,
		0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,
		0x3C,0x3D,0x3E,0x3F,0x40,0x41,0x42,0x43,0x44,0x45,
		0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
		0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,
		0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,0x60,0x61,0x62,0x63,
		0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,
		0x6E,0x6F,0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,
		0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F };
	int strLen = strlen(strVal);
	BitStream_EncodeConstraintWholeNumber(pBitStrm, strLen <= max ? strLen : max, min, max);
	Acn_Enc_String_CharIndex_private(pBitStrm, max, allowedCharSet, 128, strVal);
}


static flag Acn_Dec_String_Ascii_private(BitStream* pBitStrm,
	asn1SccSint max,
	asn1SccSint charactersToDecode,
	char* strVal)
{
	asn1SccSint i = 0;
	byte decodedCharacter;
	memset(strVal, 0x0, (size_t)max + 1);
	while (i<charactersToDecode) {
		if (!BitStream_ReadByte(pBitStrm, &decodedCharacter))
			return FALSE;
		strVal[i] = decodedCharacter;
		i++;
	}
	return TRUE;
}


flag Acn_Dec_String_Ascii_FixSize(BitStream* pBitStrm, asn1SccSint max, char* strVal)
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


flag Acn_Dec_String_Ascii_Null_Teminated(BitStream* pBitStrm, asn1SccSint max, char null_character, char* strVal)
{
	asn1SccSint i = 0;
	byte decodedCharacter;
	memset(strVal, 0x0, (size_t)max + 1);
	while (i <= max) {
		if (!BitStream_ReadByte(pBitStrm, &decodedCharacter))
			return FALSE;
		if (decodedCharacter != (byte)null_character) {
			strVal[i] = decodedCharacter;
			i++;
		}
		else {
			strVal[i] = 0x0;
			return TRUE;
		}
	}

	return FALSE;

}

flag Acn_Dec_String_Ascii_Null_Teminated_mult(BitStream* pBitStrm, asn1SccSint max, const byte null_character[], size_t null_character_size, char* strVal)
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


flag Acn_Dec_String_Ascii_External_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, asn1SccSint extSizeDeterminatFld, char* strVal)
{
	return Acn_Dec_String_Ascii_private(pBitStrm, max, extSizeDeterminatFld <= max ? extSizeDeterminatFld : max, strVal);
}

flag Acn_Dec_String_Ascii_Internal_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, asn1SccSint min, char* strVal)
{
	asn1SccSint nCount;
	if (!BitStream_DecodeConstraintWholeNumber(pBitStrm, &nCount, min, max))
		return FALSE;

	return Acn_Dec_String_Ascii_private(pBitStrm, max, nCount <= max ? nCount : max, strVal);

}

static flag Acn_Dec_String_CharIndex_private(BitStream* pBitStrm,
	asn1SccSint max,
	asn1SccSint charactersToDecode,
	byte allowedCharSet[],
	int charSetSize,
	char* strVal)
{
	asn1SccSint i = 0;
	memset(strVal, 0x0, (size_t)max + 1);
	while (i<charactersToDecode) {
		asn1SccSint charIndex = 0;
		if (!BitStream_DecodeConstraintWholeNumber(pBitStrm, &charIndex, 0, charSetSize - 1))
			return FALSE;
		strVal[i] = allowedCharSet[charIndex];
		i++;
	}
	return TRUE;
}


flag Acn_Dec_String_CharIndex_FixSize(BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, char* strVal)
{
	return Acn_Dec_String_CharIndex_private(pBitStrm, max, max, allowedCharSet, charSetSize, strVal);
}

flag Acn_Dec_String_CharIndex_External_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, asn1SccSint extSizeDeterminatFld, char* strVal)
{
	return Acn_Dec_String_CharIndex_private(pBitStrm, max, extSizeDeterminatFld <= max ? extSizeDeterminatFld : max, allowedCharSet, charSetSize, strVal);
}

flag Acn_Dec_String_CharIndex_Internal_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, asn1SccSint min, char* strVal)
{
	asn1SccSint nCount;
	if (!BitStream_DecodeConstraintWholeNumber(pBitStrm, &nCount, min, max))
		return FALSE;
	return Acn_Dec_String_CharIndex_private(pBitStrm, max, nCount <= max ? nCount : max, allowedCharSet, charSetSize, strVal);
}




flag Acn_Dec_IA5String_CharIndex_External_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, asn1SccSint extSizeDeterminatFld, char* strVal)
{
	static byte allowedCharSet[] = { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,
		0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x10,0x11,0x12,0x13,
		0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,
		0x1E,0x1F,0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,
		0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,0x30,0x31,
		0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,
		0x3C,0x3D,0x3E,0x3F,0x40,0x41,0x42,0x43,0x44,0x45,
		0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
		0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,
		0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,0x60,0x61,0x62,0x63,
		0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,
		0x6E,0x6F,0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,
		0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F };
	return Acn_Dec_String_CharIndex_private(pBitStrm, max, extSizeDeterminatFld <= max ? extSizeDeterminatFld : max, allowedCharSet, 128, strVal);
}

flag Acn_Dec_IA5String_CharIndex_Internal_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, asn1SccSint min, char* strVal)
{
	asn1SccSint nCount;
	static byte allowedCharSet[] = { 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,
		0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x10,0x11,0x12,0x13,
		0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,
		0x1E,0x1F,0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,
		0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,0x30,0x31,
		0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,
		0x3C,0x3D,0x3E,0x3F,0x40,0x41,0x42,0x43,0x44,0x45,
		0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
		0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,
		0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,0x60,0x61,0x62,0x63,
		0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,
		0x6E,0x6F,0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,
		0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F };
	if (!BitStream_DecodeConstraintWholeNumber(pBitStrm, &nCount, min, max))
		return FALSE;
	return Acn_Dec_String_CharIndex_private(pBitStrm, max, nCount <= max ? nCount : max, allowedCharSet, 128, strVal);
}






/* Length Determinant functions*/
void Acn_Enc_Length(BitStream* pBitStrm, asn1SccUint lengthValue, int lengthSizeInBits)
{
	/* encode length */
	Acn_Enc_Int_PositiveInteger_ConstSize(pBitStrm, lengthValue, lengthSizeInBits);
}

flag Acn_Dec_Length(BitStream* pBitStrm, asn1SccUint* pLengthValue, int lengthSizeInBits)
{
	return Acn_Dec_Int_PositiveInteger_ConstSize(pBitStrm, pLengthValue, lengthSizeInBits);
}

asn1SccSint milbus_encode(asn1SccSint val)
{
	return val == 32 ? 0 : val;
}

asn1SccSint milbus_decode(asn1SccSint val)
{
	return val == 0 ? 32 : val;
}


