#include <string.h>
#include <assert.h>
#include <math.h>
#include <float.h>

#include "asn1crt_encoding_uper.h"



static void ObjectIdentifier_subidentifiers_uper_encode(byte* encodingBuf, int* pSize, asn1SccUint siValue) {
	flag lastOctet = FALSE;
	byte tmp[16];
	int nSize = 0;
	int i;
	while (!lastOctet)
	{
		byte curByte = siValue % 128;
		siValue = siValue / 128;
		lastOctet = (siValue == 0);
		tmp[nSize] = curByte;
		nSize++;
	}
	for (i = 0; i < nSize; i++) {
		byte curByte = (i == nSize - 1) ? tmp[nSize - i - 1] : tmp[nSize - i - 1] | 0x80;

		encodingBuf[*pSize] = curByte;
		(*pSize)++;
	}
}


void ObjectIdentifier_uper_encode(BitStream* pBitStrm, const Asn1ObjectIdentifier *pVal) {
	//a subifentifier (i.e. a component) should not take more than size(asn1SccUint) + 2 to be encoded
	//(the above statement is true for 8 byte integers. If we ever user larger integer then it should be adjusted)
	byte tmp[OBJECT_IDENTIFIER_MAX_LENGTH * (sizeof(asn1SccUint) + 2)];
	int totalSize = 0;

	int i = 0;
	ObjectIdentifier_subidentifiers_uper_encode(tmp, &totalSize, pVal->values[0] * 40 + pVal->values[1]);
	for (i = 2; i < pVal->nCount; i++) {
		ObjectIdentifier_subidentifiers_uper_encode(tmp, &totalSize, pVal->values[i]);
	}

	if (totalSize <= 0x7F)
		BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize, 0, 0xFF);
	else
	{
		BitStream_AppendBit(pBitStrm, 1);
		BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize, 0, 0x7FFF);
	}

	for (i = 0; i < totalSize; i++) {
		BitStream_AppendByte0(pBitStrm, tmp[i]);
	}

}

void RelativeOID_uper_encode(BitStream* pBitStrm, const Asn1ObjectIdentifier *pVal) {
	//a subifentifier (i.e. a component) should not take more than size(asn1SccUint) + 2 to be encoded
	//(the above statement is true for 8 byte integers. If we ever user larger integer then it should be adjusted)
	byte tmp[OBJECT_IDENTIFIER_MAX_LENGTH * (sizeof(asn1SccUint) + 2)];
	int totalSize = 0;
	int i = 0;

	for (i = 0; i < pVal->nCount; i++) {
		ObjectIdentifier_subidentifiers_uper_encode(tmp, &totalSize, pVal->values[i]);
	}


	if (totalSize <= 0x7F)
		BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize, 0, 0xFF);
	else
	{
		BitStream_AppendBit(pBitStrm, 1);
		BitStream_EncodeConstraintWholeNumber(pBitStrm, totalSize, 0, 0x7FFF);
	}


	for (i = 0; i < totalSize; i++) {
		BitStream_AppendByte0(pBitStrm, tmp[i]);
	}
}

static flag ObjectIdentifier_subidentifiers_uper_decode(BitStream* pBitStrm, asn1SccSint* pRemainingOctets, asn1SccUint* siValue) {
	byte curByte;
	flag bLastOctet = FALSE;
	asn1SccUint curOctetValue = 0;
	*siValue = 0;
	while (*pRemainingOctets > 0 && !bLastOctet)
	{
		curByte = 0;
		if (!BitStream_ReadByte(pBitStrm, &curByte))
			return FALSE;
		(*pRemainingOctets)--;

		bLastOctet = ((curByte & 0x80) == 0);
		curOctetValue = curByte & 0x7F;
		(*siValue) <<= 7;
		(*siValue) |= curOctetValue;
	}
	return TRUE;
}

static flag ObjectIdentifier_uper_decode_lentg(BitStream* pBitStrm, asn1SccSint* totalSize) {
	asn1SccSint len2;
	if (!BitStream_DecodeConstraintWholeNumber(pBitStrm, totalSize, 0, 0xFF))
		return FALSE;
	if (*totalSize > 0x7F) {
		if (!BitStream_DecodeConstraintWholeNumber(pBitStrm, &len2, 0, 0xFF))
			return false;
		(*totalSize) <<= 8;
		(*totalSize) |= len2;
		(*totalSize) &= 0x7FFF;
	}
	return true;
}

flag ObjectIdentifier_uper_decode(BitStream* pBitStrm, Asn1ObjectIdentifier *pVal) {
	asn1SccUint si;
	asn1SccSint totalSize;
	ObjectIdentifier_Init(pVal);


	if (!ObjectIdentifier_uper_decode_lentg(pBitStrm, &totalSize))
		return FALSE;

	if (!ObjectIdentifier_subidentifiers_uper_decode(pBitStrm, &totalSize, &si))
		return FALSE;
	pVal->nCount = 2;
	pVal->values[0] = si / 40;
	pVal->values[1] = si % 40;
	while (totalSize > 0 && pVal->nCount < OBJECT_IDENTIFIER_MAX_LENGTH)
	{
		if (!ObjectIdentifier_subidentifiers_uper_decode(pBitStrm, &totalSize, &si))
			return FALSE;

		pVal->values[pVal->nCount] = si;
		pVal->nCount++;
	}
	//return true, if totalSize reduced to zero. Otherwise we exit the loop because more components we present than OBJECT_IDENTIFIER_MAX_LENGTH
	return totalSize == 0;
}

flag RelativeOID_uper_decode(BitStream* pBitStrm, Asn1ObjectIdentifier *pVal) {
	asn1SccUint si;
	asn1SccSint totalSize;
	ObjectIdentifier_Init(pVal);

	if (!ObjectIdentifier_uper_decode_lentg(pBitStrm, &totalSize))
		return FALSE;

	while (totalSize > 0 && pVal->nCount < OBJECT_IDENTIFIER_MAX_LENGTH)
	{
		if (!ObjectIdentifier_subidentifiers_uper_decode(pBitStrm, &totalSize, &si))
			return FALSE;
		pVal->values[pVal->nCount] = si;
		pVal->nCount++;
	}
	//return true, if totalSize is zero. Otherwise we exit the loop because more components were present than OBJECT_IDENTIFIER_MAX_LENGTH
	return totalSize == 0;
}

