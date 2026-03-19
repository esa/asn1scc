#include <string.h>
#include <assert.h>

#include "asn1crt_encoding_acn.h"









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








void Acn_Enc_Int_TwosComplement_ConstSize_big_endian_32(BitStream* pBitStrm, asn1SccSint intVal)
{
	Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm, int2uint(intVal));
}












flag Acn_Dec_Int_TwosComplement_ConstSize(BitStream* pBitStrm, asn1SccSint* pIntVal, int encodedSizeInBits)
{


	int i;
	flag valIsNegative = BitStream_PeekBit(pBitStrm);
	int nBytes = encodedSizeInBits / 8;
	int rstBits = encodedSizeInBits % 8;
	byte b = 0;

	*pIntVal = valIsNegative ? MAX_INT : 0;
	for (i = 0; i<nBytes; i++) {
		if (!BitStream_ReadByte(pBitStrm, &b))
			return FALSE;
		*pIntVal = (*pIntVal << 8) | b;
	}

	if (rstBits>0)
	{
		if (!BitStream_ReadPartialByte(pBitStrm, &b, (byte)rstBits))
			return FALSE;
		*pIntVal = (*pIntVal << rstBits) | b;
	}
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





















//return values is in nibbles


















//encoding puts an 'F' at the end















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



























/* Boolean Decode */

/**
 * Reads nBitsToRead bits from the BitStream and compares them with the bitPattern to determine the boolean value.
 * If the read bits match the bitPattern, the function returns TRUE and sets the pBoolValue to TRUE.
 * If the read bits do not match the bitPattern, the function returns TRUE but sets the pBoolValue to FALSE.
 * If the are not enough bits in the BitStream to read nBitsToRead, the function returns FALSE.
 * 
 * @param pBitStrm The BitStream from which to read the bits.
 * @param bitPattern The pattern to compare the read bits with.
 * @param nBitsToRead The number of bits to read from the BitStream.
 * @param pBoolValue A pointer to the variable where the decoded boolean value will be stored.
 */

flag BitStream_ReadBitPattern(BitStream* pBitStrm, const byte* bitPattern, int nBitsToRead, flag* pBoolValue)
{
	int nBytesToRead = nBitsToRead / 8;
	int nRemainingBitsToRead = nBitsToRead % 8;
	byte curByte;
	int i = 0;

	*pBoolValue = TRUE;
	for (i = 0; i<nBytesToRead; i++) {
		if (!BitStream_ReadByte(pBitStrm, &curByte))
			return FALSE;
        *pBoolValue = *pBoolValue && (curByte == bitPattern[i]);
	}

	if (nRemainingBitsToRead > 0) {
		if (!BitStream_ReadPartialByte(pBitStrm, &curByte, (byte)nRemainingBitsToRead))
			return FALSE;
        *pBoolValue = *pBoolValue && (curByte == bitPattern[nBytesToRead] >> (8 - nRemainingBitsToRead));
	}

	return TRUE;
}



/**
 * Decodes a boolean value from a BitStream using the ACN encoding when both true and false bit patterns are provided.
 * The function reads nBitsToRead bits from the BitStream and compares them with the truePattern and falsePattern to determine the boolean value.
 * If the read bits match the truePattern, the function returns TRUE and sets the pBoolValue to TRUE.
 * If the read bits match the falsePattern, the function returns TRUE and sets the pBoolValue to FALSE.
 * If the read bits do not match either pattern, the function returns FALSE and sets the pBoolValue to FALSE.
 *
 * @param pBitStrm The BitStream from which to decode the boolean value.
 * @param truePattern The pattern representing the true value in the ACN encoding.
 * @param falsePattern The pattern representing the false value in the ACN encoding.
 * @param nBitsToRead The number of bits to read from the BitStream.
 * @param pBoolValue A pointer to the variable where the decoded boolean value will be stored.
 * @return Returns TRUE if the boolean value was successfully decoded, FALSE otherwise.
 */



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















/* String functions*/

































































/* Length Determinant functions*/




































































































































































































































