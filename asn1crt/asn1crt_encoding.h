#ifndef ASN1SCC_ASN1CRT_ENCODING_H_
#define ASN1SCC_ASN1CRT_ENCODING_H_

#include "asn1crt.h"

#ifdef  __cplusplus
extern "C" {
#endif

/* Bit strean functions */

void BitStream_AppendNBitZero(BitStream* pBitStrm, int nbits);
void BitStream_EncodeNonNegativeInteger(BitStream* pBitStrm, asn1SccUint v);
void BitStream_AppendNBitOne(BitStream* pBitStrm, int nbits);
void BitStream_EncodeNonNegativeIntegerNeg(BitStream* pBitStrm, asn1SccUint v, flag negate);
flag BitStream_DecodeNonNegativeInteger(BitStream* pBitStrm, asn1SccUint* v, int nBits);
flag BitStream_ReadPartialByte(BitStream* pBitStrm, byte *v, byte nbits);
void BitStream_AppendPartialByte(BitStream* pBitStrm, byte v, byte nbits, flag negate);





void BitStream_Init(BitStream* pBitStrm, unsigned char* buf, long count);
void BitStream_AttachBuffer(BitStream* pBitStrm, unsigned char* buf, long count);
void BitStream_AppendBit(BitStream* pBitStrm, flag v);
void BitStream_AppendBits(BitStream* pBitStrm, const byte* srcBuffer, int nBitsToWrite);
void BitStream_AppendByte(BitStream* pBitStrm, byte v, flag negate);
void BitStream_AppendByte0(BitStream* pBitStrm, byte v);


void ByteStream_Init(ByteStream* pStrm, byte* buf, long count);
void ByteStream_AttachBuffer(ByteStream* pStrm, unsigned char* buf, long count);
asn1SccSint ByteStream_GetLength(ByteStream* pStrm);

asn1SccSint BitStream_GetLength(BitStream* pBitStrm);
void BitStream_AppendBitOne(BitStream* pBitStrm);
void BitStream_AppendBitZero(BitStream* pBitStrm);
flag BitStream_ReadBit(BitStream* pBitStrm, flag* v);
flag BitStream_ReadBits(BitStream* pBitStrm, byte* BuffToWrite, int nBitsToRead);
flag BitStream_ReadByte(BitStream* pBitStrm, byte* v);

/* Integer functions */


void BitStream_EncodeUnConstraintWholeNumber(BitStream* pBitStrm, asn1SccSint v);
void BitStream_EncodeSemiConstraintWholeNumber(BitStream* pBitStrm, asn1SccSint v, asn1SccSint min);
void BitStream_EncodeSemiConstraintPosWholeNumber(BitStream* pBitStrm, asn1SccUint v, asn1SccUint min);
void BitStream_EncodeConstraintWholeNumber(BitStream* pBitStrm, asn1SccSint v, asn1SccSint min, asn1SccSint max);
void BitStream_EncodeConstraintPosWholeNumber(BitStream* pBitStrm, asn1SccUint v, asn1SccUint min, asn1SccUint max);

flag BitStream_DecodeUnConstraintWholeNumber(BitStream* pBitStrm, asn1SccSint* v);
flag BitStream_DecodeSemiConstraintWholeNumber(BitStream* pBitStrm, asn1SccSint* v, asn1SccSint min);
flag BitStream_DecodeSemiConstraintPosWholeNumber(BitStream* pBitStrm, asn1SccUint* v, asn1SccUint min);
flag BitStream_DecodeConstraintWholeNumber(BitStream* pBitStrm, asn1SccSint* v, asn1SccSint min, asn1SccSint max);
flag BitStream_DecodeConstraintPosWholeNumber(BitStream* pBitStrm, asn1SccUint* v, asn1SccUint min, asn1SccUint max);

int GetNumberOfBitsForNonNegativeInteger(asn1SccUint v);

void CalculateMantissaAndExponent(asn1Real d, int* exp, asn1SccUint64* mantissa);
asn1Real GetDoubleByMantissaAndExp(asn1SccUint mantissa, int exp);

int GetLengthInBytesOfSInt(asn1SccSint v); 
int GetLengthInBytesOfUInt(asn1SccUint64 v);

void BitStream_EncodeReal(BitStream* pBitStrm, asn1Real v);
flag BitStream_DecodeReal(BitStream* pBitStrm, asn1Real* v);



#ifdef  __cplusplus
}
#endif

#endif
