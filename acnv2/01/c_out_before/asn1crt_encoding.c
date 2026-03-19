#include <string.h>
#include <assert.h>
#include <math.h>
#include <float.h>

#include "asn1crt_encoding.h"



const byte masks[] = { 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 };
const byte masksb[] = { 0x0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF };

const asn1SccUint32 masks2[] = { 0x0,
0xFF,
0xFF00,
0xFF0000,
0xFF000000 };

flag OctetString_equal(int len1, int len2, const byte arr1[], const byte arr2[])
{
	return (len1 == len2) && (memcmp(arr1, arr2, len1) == 0);
}

/***********************************************************************************************/
/*   Byte Stream Functions                                                                      */
/***********************************************************************************************/




asn1SccSint ByteStream_GetLength(ByteStream* pStrm)
{
    return pStrm->currentByte;
}


/***********************************************************************************************/
/*   Bit Stream Functions                                                                      */
/***********************************************************************************************/




void BitStream_Init(BitStream* pBitStrm, unsigned char* buf, long count)
{
	pBitStrm->count = count;
	pBitStrm->buf = buf;
	memset(pBitStrm->buf, 0x0, (size_t)count);
	pBitStrm->currentByte = 0;
	pBitStrm->currentBit = 0;
	pBitStrm->pushDataPrm = NULL;
	pBitStrm->fetchDataPrm = NULL;
}



void BitStream_AttachBuffer(BitStream* pBitStrm, unsigned char* buf, long count)
{
	pBitStrm->count = count;
	pBitStrm->buf = buf;
	pBitStrm->currentByte = 0;
	pBitStrm->currentBit = 0;
	pBitStrm->pushDataPrm = NULL;
	pBitStrm->fetchDataPrm = NULL;
}

void BitStream_AttachBuffer2(BitStream* pBitStrm, unsigned char* buf, long count, void* pushDataPrm, void* fetchDataPrm)
{
	BitStream_AttachBuffer(pBitStrm, buf, count);
	pBitStrm->pushDataPrm = pushDataPrm;
	pBitStrm->fetchDataPrm = fetchDataPrm;
}


asn1SccSint BitStream_GetLength(BitStream* pBitStrm)
{
	int ret = pBitStrm->currentByte;
	if (pBitStrm->currentBit)
		ret++;
	return ret;
}

/*
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
*/



/*
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
*/



void BitStream_AppendNBitZero(BitStream* pBitStrm, int nbits)
{
	int totalBits = pBitStrm->currentBit + nbits;
	int totalBytes = totalBits / 8;
	pBitStrm->currentBit = totalBits % 8;
	//pBitStrm->currentByte += totalBits / 8;
	if (pBitStrm->currentByte + totalBytes <= pBitStrm->count) {
		pBitStrm->currentByte += totalBytes;
		bitstream_push_data_if_required(pBitStrm);
	} else {
		int extraBytes = pBitStrm->currentByte + totalBytes - pBitStrm->count;
		pBitStrm->currentByte = pBitStrm->count;
		bitstream_push_data_if_required(pBitStrm);
		pBitStrm->currentByte = extraBytes;
	}
}





void BitStream_AppendBit(BitStream* pBitStrm, flag v)
{
	if (v) {
		pBitStrm->buf[pBitStrm->currentByte] |= masks[pBitStrm->currentBit];
	}
	else {
		byte nmask = (byte)~masks[pBitStrm->currentBit];
		pBitStrm->buf[pBitStrm->currentByte] &= nmask;
	}

	if (pBitStrm->currentBit<7)
		pBitStrm->currentBit++;
	else {
		pBitStrm->currentBit = 0;
		pBitStrm->currentByte++;
		bitstream_push_data_if_required(pBitStrm);
	}
	assert(pBitStrm->currentByte * 8 + pBitStrm->currentBit <= pBitStrm->count * 8);
}


flag BitStream_ReadBit(BitStream* pBitStrm, flag* v)
{
	*v = pBitStrm->buf[pBitStrm->currentByte] & masks[pBitStrm->currentBit];

	if (pBitStrm->currentBit<7)
		pBitStrm->currentBit++;
	else {
		pBitStrm->currentBit = 0;
		pBitStrm->currentByte++;
		bitstream_fetch_data_if_required(pBitStrm);
	}
	return pBitStrm->currentByte * 8 + pBitStrm->currentBit <= pBitStrm->count * 8;
}




/*
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

*/


void BitStream_AppendByte(BitStream* pBitStrm, byte v, flag negate)
{
	//static byte masksb[] = { 0x0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF };
	int cb = pBitStrm->currentBit;
	int ncb = 8 - cb;
	if (negate)
		v = (byte)~v;
	byte mask = (byte)~masksb[ncb];

	pBitStrm->buf[pBitStrm->currentByte] &= mask;
	pBitStrm->buf[pBitStrm->currentByte++] |= (byte)(v >> cb);
	bitstream_push_data_if_required(pBitStrm);
	assert(pBitStrm->currentByte * 8 + pBitStrm->currentBit <= pBitStrm->count * 8);

	if (cb) {
		mask = (byte)~mask;
		pBitStrm->buf[pBitStrm->currentByte] &= mask;
		pBitStrm->buf[pBitStrm->currentByte] |= (byte)(v << ncb);
	}

}

flag BitStream_AppendByte0(BitStream* pBitStrm, byte v)
{
	int cb = pBitStrm->currentBit;
	int ncb = 8 - cb;

	byte mask = (byte)~masksb[ncb];

	pBitStrm->buf[pBitStrm->currentByte] &= mask;
	pBitStrm->buf[pBitStrm->currentByte++] |= (byte)(v >> cb);
	bitstream_push_data_if_required(pBitStrm);

	if (cb) {
		if (pBitStrm->currentByte >= pBitStrm->count)
			return FALSE;
		mask = (byte)~mask;
		pBitStrm->buf[pBitStrm->currentByte] &= mask;
		pBitStrm->buf[pBitStrm->currentByte] |= (byte)(v << ncb);
	}
	return TRUE;
}

flag BitStream_AppendByteArray(BitStream* pBitStrm, const byte arr[], const int arr_len)
{
    //static byte  masks[] = { 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 };
    //static byte masksb[] = { 0x00, 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F, 0xFF };

    int cb = pBitStrm->currentBit;
    int ncb = 8 - cb;

    byte mask = (byte)~masksb[ncb];
    byte nmask = (byte)~mask;
    //if (pBitStrm->currentByte + (int)arr_len + (cb > 0 ? 1 : 0) >= pBitStrm->count)
    if ( (pBitStrm->currentByte + arr_len)*8 + cb > pBitStrm->count*8)
        return FALSE;

    if (arr_len> 0) {
        byte v = arr[0];
        pBitStrm->buf[pBitStrm->currentByte] &= mask;       //make zero right bits (i.e. the ones that will get the new value)
        pBitStrm->buf[pBitStrm->currentByte++] |= (byte)(v >> cb);  //shift right and then populate current byte
        bitstream_push_data_if_required(pBitStrm);

        pBitStrm->buf[pBitStrm->currentByte] &= nmask;
        pBitStrm->buf[pBitStrm->currentByte] |= (byte)(v << ncb);
    }

    for (int i = 1; i < arr_len - 1; i++) {
        byte v = arr[i];
        byte v1 = (byte)(v >> cb);
        byte v2 = (byte)(v << ncb);
        pBitStrm->buf[pBitStrm->currentByte++] |= v1;  //shift right and then populate current byte
        bitstream_push_data_if_required(pBitStrm);
        pBitStrm->buf[pBitStrm->currentByte] |= v2;
    }
    if (arr_len - 1 > 0) {
        byte v = arr[arr_len - 1];
        pBitStrm->buf[pBitStrm->currentByte] &= mask;       //make zero right bits (i.e. the ones that will get the new value)
        pBitStrm->buf[pBitStrm->currentByte++] |= (byte)(v >> cb);  //shift right and then populate current byte
        bitstream_push_data_if_required(pBitStrm);

        if (cb) {
            pBitStrm->buf[pBitStrm->currentByte] &= nmask;
            pBitStrm->buf[pBitStrm->currentByte] |= (byte)(v << ncb);
        }
    }
    return TRUE;
}


flag BitStream_ReadByte(BitStream* pBitStrm, byte* v)
{
    int cb = pBitStrm->currentBit;			//bit position in the current byte

    //check if the available bytes are enough
	int requiredBytes = (cb > 0) ? 2 : 1;
	int availableBytes = pBitStrm->count - pBitStrm->currentByte;

	if (availableBytes < requiredBytes) {
		return FALSE;
	}

	*v = (byte)(pBitStrm->buf[pBitStrm->currentByte++] << cb);
	bitstream_fetch_data_if_required(pBitStrm);

	if (cb > 0) {
        // we need to read bits from the next byte
		int ncb = 8 - cb;						
		*v |= (byte)(pBitStrm->buf[pBitStrm->currentByte] >> ncb);
	}

	return TRUE;
}




flag BitStream_ReadByteArray(BitStream* pBitStrm, byte* arr, int arr_len) {
    int cb = pBitStrm->currentBit;
    int ncb = 8 - cb;
	byte* rb = &pBitStrm->buf[pBitStrm->currentByte];
	byte* wb = arr;

#ifndef ASN1SCC_STREAMING
    if ( (pBitStrm->currentByte + arr_len)*8 + cb > pBitStrm->count*8)
        return FALSE;
#endif

    for (int i = 0; i < arr_len; i++) {
		*wb = (byte)((*rb) << cb);
		rb++;
		bitstream_fetch_data_if_required(pBitStrm);
		*wb |= (byte)((*rb) >> ncb);
		wb++;
    }
	pBitStrm->currentByte += arr_len;
    return TRUE;
}


/*
flag BitStream_ReadByte2(BitStream2* pBitStrm, byte* v)
{
	int cb = pBitStrm->currentBit;
	int ncb = 8 - pBitStrm->currentBit;
	*v = (byte)(pBitStrm->buf[pBitStrm->currentByte++] << cb);

	if (pBitStrm->currentByte == pBitStrm->count && pBitStrm->fetchData != NULL) {
		pBitStrm->fetchData(pBitStrm);
	}

	if (cb) {
		*v |= (byte)(pBitStrm->buf[pBitStrm->currentByte] >> ncb);
	}

	return pBitStrm->currentByte * 8 + pBitStrm->currentBit <= pBitStrm->count * 8;
}

*/



/* nbits 1..7*/
void BitStream_AppendPartialByte(BitStream* pBitStrm, byte v, byte nbits, flag negate)
{
	int cb = pBitStrm->currentBit;
	int totalBits = cb + nbits;
	int ncb = 8 - cb;
	int totalBitsForNextByte;
	if (negate)
		v = masksb[nbits] & ((byte)~v);
	byte mask1 = (byte)~masksb[ncb];

	if (totalBits <= 8) {
		//static byte masksb[] = { 0x0, 0x1, 0x3, 0x7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF };
		byte mask2 = masksb[8 - totalBits];
		byte mask = mask1 | mask2;
		//e.g. current bit = 3 --> mask =  1110 0000
		//nbits = 3 --> totalBits = 6
		//                         mask=   1110 0000
		//                         and     0000 0011 <- masks[totalBits - 1]
        //	                              -----------
		//					final mask     1110 0011
		pBitStrm->buf[pBitStrm->currentByte] &= mask;
		pBitStrm->buf[pBitStrm->currentByte] |= (byte)(v << (8 - totalBits));
		pBitStrm->currentBit += nbits;
		if (pBitStrm->currentBit == 8) {
			pBitStrm->currentBit = 0;
			pBitStrm->currentByte++;
			bitstream_push_data_if_required(pBitStrm);
		}
	}
	else {
		totalBitsForNextByte = totalBits - 8;
		pBitStrm->buf[pBitStrm->currentByte] &= mask1;
		pBitStrm->buf[pBitStrm->currentByte++] |= (byte)(v >> totalBitsForNextByte);
		bitstream_push_data_if_required(pBitStrm);
		byte mask = (byte)~masksb[8 - totalBitsForNextByte];
		pBitStrm->buf[pBitStrm->currentByte] &= mask;
		pBitStrm->buf[pBitStrm->currentByte] |= (byte)(v << (8 - totalBitsForNextByte));
		pBitStrm->currentBit = totalBitsForNextByte;
	}
	assert(pBitStrm->currentByte * 8 + pBitStrm->currentBit <= pBitStrm->count * 8);

}

/* nbits 1..7*/
flag BitStream_ReadPartialByte(BitStream* pBitStrm, byte *v, byte nbits)
{
	int cb = pBitStrm->currentBit;
	int totalBits = cb + nbits;
	int totalBitsForNextByte;

	if (totalBits <= 8) {
		*v = (byte)((pBitStrm->buf[pBitStrm->currentByte] >> (8 - totalBits)) & masksb[nbits]);
		pBitStrm->currentBit += nbits;
		if (pBitStrm->currentBit == 8) {
			pBitStrm->currentBit = 0;
			pBitStrm->currentByte++;
			bitstream_fetch_data_if_required(pBitStrm);
		}
	}
	else {
		totalBitsForNextByte = totalBits - 8;
		*v = (byte)(pBitStrm->buf[pBitStrm->currentByte++] << totalBitsForNextByte);
		bitstream_fetch_data_if_required(pBitStrm);
		*v |= (byte)(pBitStrm->buf[pBitStrm->currentByte] >> (8 - totalBitsForNextByte));
		*v &= masksb[nbits];
		pBitStrm->currentBit = totalBitsForNextByte;
	}
	return pBitStrm->currentByte * 8 + pBitStrm->currentBit <= pBitStrm->count * 8;
}




/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/
/*   Integer Functions                                                                     */
/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/
/***********************************************************************************************/



static void BitStream_EncodeNonNegativeInteger32Neg(BitStream* pBitStrm,
	asn1SccUint32 v,
	flag negate)
{
	int cc;
	asn1SccUint32 curMask;
	int pbits;

	if (v == 0)
		return;

	if (v<0x100) {
		cc = 8;
		curMask = 0x80;
	}
	else if (v<0x10000) {
		cc = 16;
		curMask = 0x8000;
	}
	else if (v<0x1000000) {
		cc = 24;
		curMask = 0x800000;
	}
	else {
		cc = 32;
		curMask = 0x80000000;
	}

	while ((v & curMask) == 0) {
		curMask >>= 1;
		cc--;
	}

	pbits = cc % 8;
	if (pbits) {
		cc -= pbits;
		BitStream_AppendPartialByte(pBitStrm, (byte)(v >> cc), (byte)pbits, negate);
	}

	while (cc) {
		asn1SccUint32 t1 = v & masks2[cc >> 3];
		cc -= 8;
		BitStream_AppendByte(pBitStrm, (byte)(t1 >> cc), negate);
	}

}

static flag BitStream_DecodeNonNegativeInteger32Neg(BitStream* pBitStrm,
	asn1SccUint32* v,
	int nBits)
{
	byte b;
	*v = 0;
	while (nBits >= 8) {
		*v <<= 8;
		if (!BitStream_ReadByte(pBitStrm, &b))
			return FALSE;
		*v |= b;
		nBits -= 8;
	}
	if (nBits)
	{
		*v <<= nBits;
		if (!BitStream_ReadPartialByte(pBitStrm, &b, (byte)nBits))
			return FALSE;
		*v |= b;
	}

	return TRUE;
}



void BitStream_EncodeNonNegativeInteger(BitStream* pBitStrm, asn1SccUint v)
{

#if WORD_SIZE==8
	if (v<0x100000000LL)
		BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, (asn1SccUint32)v, 0);
	else {
		asn1SccUint32 hi = (asn1SccUint32)(v >> 32);
		asn1SccUint32 lo = (asn1SccUint32)v;
		int nBits;
		BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, hi, 0);

		nBits = GetNumberOfBitsForNonNegativeInteger(lo);
		BitStream_AppendNBitZero(pBitStrm, 32 - nBits);
		BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, lo, 0);
	}
#else
	BitStream_EncodeNonNegativeInteger32Neg(pBitStrm, v, 0);
#endif
}


flag BitStream_DecodeNonNegativeInteger(BitStream* pBitStrm, asn1SccUint* v, int nBits)
{
#if WORD_SIZE==8
	asn1SccUint32 hi = 0;
	asn1SccUint32 lo = 0;
	flag ret;

	if (nBits <= 32)
	{
		ret = BitStream_DecodeNonNegativeInteger32Neg(pBitStrm, &lo, nBits);
		*v = lo;
		return ret;
	}

	ret = BitStream_DecodeNonNegativeInteger32Neg(pBitStrm, &hi, 32) && BitStream_DecodeNonNegativeInteger32Neg(pBitStrm, &lo, nBits - 32);

	*v = hi;
	*v <<= nBits - 32;
	*v |= lo;
	return ret;
#else
	return BitStream_DecodeNonNegativeInteger32Neg(pBitStrm, v, nBits);
#endif
}




int GetNumberOfBitsForNonNegativeInteger32(asn1SccUint32 v)
{
	int ret = 0;

	if (v<0x100) {
		ret = 0;
	}
	else if (v<0x10000) {
		ret = 8;
		v >>= 8;
	}
	else if (v<0x1000000) {
		ret = 16;
		v >>= 16;
	}
	else {
		ret = 24;
		v >>= 24;
	}
	while (v>0) {
		v >>= 1;
		ret++;
	}
	return ret;
}

int GetNumberOfBitsForNonNegativeInteger(asn1SccUint v)
{
#if WORD_SIZE==8
	if (v<0x100000000LL)
		return GetNumberOfBitsForNonNegativeInteger32((asn1SccUint32)v);
	else {
		asn1SccUint32 hi = (asn1SccUint32)(v >> 32);
		return 32 + GetNumberOfBitsForNonNegativeInteger32(hi);
	}
#else
	return GetNumberOfBitsForNonNegativeInteger32(v);
#endif
}









void BitStream_EncodeConstraintWholeNumber(BitStream* pBitStrm, asn1SccSint v, asn1SccSint min, asn1SccSint max)
{
	int nRangeBits;
	int nBits;
	asn1SccUint range;
	assert(min <= max);
	range = (asn1SccUint)(max - min);
	if (!range)
		return;
	nRangeBits = GetNumberOfBitsForNonNegativeInteger(range);
	nBits = GetNumberOfBitsForNonNegativeInteger((asn1SccUint)(v - min));
	BitStream_AppendNBitZero(pBitStrm, nRangeBits - nBits);
	BitStream_EncodeNonNegativeInteger(pBitStrm, (asn1SccUint)(v - min));
}

void BitStream_EncodeConstraintPosWholeNumber(BitStream* pBitStrm, asn1SccUint v, asn1SccUint min, asn1SccUint max)
{
	int nRangeBits;
	int nBits;
	asn1SccUint range;
	assert(min <= v);
	assert(v <= max);
	range = (asn1SccUint)(max - min);
	if (!range)
		return;
	nRangeBits = GetNumberOfBitsForNonNegativeInteger(range);
	nBits = GetNumberOfBitsForNonNegativeInteger(v - min);
	BitStream_AppendNBitZero(pBitStrm, nRangeBits - nBits);
	BitStream_EncodeNonNegativeInteger(pBitStrm, v - min);
}


flag BitStream_DecodeConstraintWholeNumber(BitStream* pBitStrm, asn1SccSint* v, asn1SccSint min, asn1SccSint max)
{
	asn1SccUint uv;
	int nRangeBits;
	asn1SccUint range = (asn1SccUint)(max - min);

	ASSERT_OR_RETURN_FALSE(min <= max);


	*v = 0;
	if (!range) {
		*v = min;
		return TRUE;
	}

	nRangeBits = GetNumberOfBitsForNonNegativeInteger(range);


	if (BitStream_DecodeNonNegativeInteger(pBitStrm, &uv, nRangeBits))
	{
		*v = ((asn1SccSint)uv) + min;
		return TRUE;
	}
	return FALSE;
}


















flag BitStream_DecodeConstraintPosWholeNumber(BitStream* pBitStrm, asn1SccUint* v, asn1SccUint min, asn1SccUint max)
{
	asn1SccUint uv;
	int nRangeBits;
	asn1SccUint range = max - min;

	ASSERT_OR_RETURN_FALSE(min <= max);


	*v = 0;
	if (!range) {
		*v = min;
		return TRUE;
	}

	nRangeBits = GetNumberOfBitsForNonNegativeInteger(range);

	if (BitStream_DecodeNonNegativeInteger(pBitStrm, &uv, nRangeBits))
	{
		*v = uv + min;
		return TRUE;
	}
	return FALSE;
}


















#ifndef INFINITY
#ifdef __GNUC__
#define INFINITY (__builtin_inf())
#endif
#endif

/*
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
*/

#if FP_WORD_SIZE==8

#define ExpoBitMask  0x7FF0000000000000ULL
#define MantBitMask  0x000FFFFFFFFFFFFFULL
#define MantBitMask2 0xFFE0000000000000ULL
#define MantisaExtraBit 0x0010000000000000ULL
#else

#define ExpoBitMask  0x7F800000U
#define MantBitMask  0x007FFFFFU
#define MantBitMask2 0xF0000000U
#define MantisaExtraBit 0x00800000U

#endif











flag DecodeRealAsBinaryEncoding(BitStream* pBitStrm, int length, byte header, asn1Real* v);












flag BitStream_EncodeOctetString_no_length (BitStream* pBitStrm, const byte* arr, int nCount) {
	int cb = pBitStrm->currentBit;
	//int i1;
	flag ret = TRUE;

	if (cb == 0) {
#ifdef ASN1SCC_STREAMING
		int remainingBytesToSend = nCount;
		while (remainingBytesToSend > 0) {
			int currentBatch =
				pBitStrm->currentByte + remainingBytesToSend <= pBitStrm->count ?
				remainingBytesToSend :
				pBitStrm->count - pBitStrm->currentByte;

			memcpy(&pBitStrm->buf[pBitStrm->currentByte], arr, currentBatch);
			pBitStrm->currentByte += currentBatch;
			bitstream_push_data_if_required(pBitStrm);
			remainingBytesToSend -= currentBatch;
		}
#else
		ret = pBitStrm->currentByte + nCount <= pBitStrm->count;
		if (ret) {
			memcpy(&pBitStrm->buf[pBitStrm->currentByte], arr, nCount);
			pBitStrm->currentByte += nCount;
		}
#endif

	}
	else {
		ret = BitStream_AppendByteArray(pBitStrm, arr, (size_t) nCount);
/*
		for (i1 = 0; (i1 < (int)nCount) && ret; i1++)
		{
			ret = BitStream_AppendByte0(pBitStrm, arr[i1]);
		}
*/		
	}

	return ret;
}

flag BitStream_DecodeOctetString_no_length(BitStream* pBitStrm, byte* arr, int nCount) {
    int cb = pBitStrm->currentBit;
    //int i1;
	flag ret=TRUE;

    if (cb == 0) {
#ifdef ASN1SCC_STREAMING
        int remainingBytesToRead = nCount;
        while (remainingBytesToRead > 0) {
            int currentBatch =
                pBitStrm->currentByte + remainingBytesToRead <= pBitStrm->count ?
                remainingBytesToRead :
                pBitStrm->count - pBitStrm->currentByte;

            memcpy(arr, &pBitStrm->buf[pBitStrm->currentByte], currentBatch);
            pBitStrm->currentByte += currentBatch;
            bitstream_fetch_data_if_required(pBitStrm);
            remainingBytesToRead -= currentBatch;
        }
#else
        ret = pBitStrm->currentByte + nCount <= pBitStrm->count;
        if (ret) {
            memcpy(arr, &pBitStrm->buf[pBitStrm->currentByte], nCount);
            pBitStrm->currentByte += nCount;
        }
#endif

    }
    else {
		ret = BitStream_ReadByteArray(pBitStrm, arr, nCount);
        //for (i1 = 0; (i1 < nCount) && ret; i1++)
        //{
        //    ret = BitStream_ReadByte(pBitStrm, &arr[i1]);
        //}
    }

	return ret;
}



flag BitStream_EncodeOctetString_fragmentation(BitStream* pBitStrm, const byte* arr, int nCount) {
	int i1;
	int nRemainingItemsVar1 = nCount;
	int nCurBlockSize1 = 0;
	int nCurOffset1 = 0;
	flag ret = nCount >= 0;

	while (nRemainingItemsVar1 >= 0x4000 && ret)
	{
		if (nRemainingItemsVar1 >= 0x10000)
		{
			nCurBlockSize1 = 0x10000;
			BitStream_EncodeConstraintWholeNumber(pBitStrm, 0xC4, 0, 0xFF);
		}
		else if (nRemainingItemsVar1 >= 0xC000)
		{
			nCurBlockSize1 = 0xC000;
			BitStream_EncodeConstraintWholeNumber(pBitStrm, 0xC3, 0, 0xFF);
		}
		else if (nRemainingItemsVar1 >= 0x8000)
		{
			nCurBlockSize1 = 0x8000;
			BitStream_EncodeConstraintWholeNumber(pBitStrm, 0xC2, 0, 0xFF);
		}
		else
		{
			nCurBlockSize1 = 0x4000;
			BitStream_EncodeConstraintWholeNumber(pBitStrm, 0xC1, 0, 0xFF);
		}

		for (i1 = nCurOffset1; (i1 < nCurBlockSize1 + nCurOffset1) && ret; i1++)
		{
			ret = BitStream_AppendByte0(pBitStrm, arr[i1]);
		}
		nCurOffset1 += nCurBlockSize1;
		nRemainingItemsVar1 -= nCurBlockSize1;
	}
	if (ret) {
		if (nRemainingItemsVar1 <= 0x7F)
			BitStream_EncodeConstraintWholeNumber(pBitStrm, nRemainingItemsVar1, 0, 0xFF);
		else
		{
			BitStream_AppendBit(pBitStrm, 1);
			BitStream_EncodeConstraintWholeNumber(pBitStrm, nRemainingItemsVar1, 0, 0x7FFF);
		}

		for (i1 = nCurOffset1; i1 < (nCurOffset1 + nRemainingItemsVar1) && ret; i1++)
		{
			ret = BitStream_AppendByte0(pBitStrm, arr[i1]);
		}
	}
	return ret;
}

flag BitStream_DecodeOctetString_fragmentation(BitStream* pBitStrm, byte* arr, int* nCount, asn1SccSint asn1SizeMax) {
	flag ret = TRUE;

	int i1;
	asn1SccSint nLengthTmp1 = 0;
	asn1SccSint nRemainingItemsVar1 = 0;
	asn1SccSint nCurBlockSize1 = 0;
	asn1SccSint nCurOffset1 = 0;

	ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, &nRemainingItemsVar1, 0, 0xFF);
	if (ret) {
		while (ret && (nRemainingItemsVar1 & 0xC0) == 0xC0)
		{
			if (nRemainingItemsVar1 == 0xC4)
				nCurBlockSize1 = 0x10000;
			else if (nRemainingItemsVar1 == 0xC3)
				nCurBlockSize1 = 0xC000;
			else if (nRemainingItemsVar1 == 0xC2)
				nCurBlockSize1 = 0x8000;
			else if (nRemainingItemsVar1 == 0xC1)
				nCurBlockSize1 = 0x4000;
			else {
				ret =  FALSE;
			}
			if (ret) {
				ret = nCurOffset1 + nCurBlockSize1 <= asn1SizeMax;
				for (i1 = (int)nCurOffset1; ret && (i1 < (int)(nCurOffset1 + nCurBlockSize1)); i1++)
				{
					ret = BitStream_ReadByte(pBitStrm, &(arr[i1]));
				}

				if (ret) {
					nLengthTmp1 += (long)nCurBlockSize1;
					nCurOffset1 += nCurBlockSize1;
					ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, &nRemainingItemsVar1, 0, 0xFF);
				}
			}
		}
		if (ret) {
			if ((nRemainingItemsVar1 & 0x80)>0)
			{
				asn1SccSint len2;
				nRemainingItemsVar1 <<= 8;
				ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, &len2, 0, 0xFF);
				if (ret) {
					nRemainingItemsVar1 |= len2;
					nRemainingItemsVar1 &= 0x7FFF;
				}
			}
			ret = ret && (nCurOffset1 + nRemainingItemsVar1 <= asn1SizeMax);
			if (ret) {
				for (i1 = (int)nCurOffset1; ret && (i1 < (int)(nCurOffset1 + nRemainingItemsVar1)); i1++)
				{
					ret = BitStream_ReadByte(pBitStrm, &(arr[i1]));
				}
				if (ret) {
					nLengthTmp1 += (long)nRemainingItemsVar1;

					if ((nLengthTmp1 >= 1) && (nLengthTmp1 <= asn1SizeMax)) {
						*nCount = (int)nLengthTmp1;

					}
					else {
						ret = FALSE;  /*COVERAGE_IGNORE*/
					}

				}
			}
		}
	}

	return ret;
}



flag BitStream_EncodeOctetString(BitStream* pBitStrm, const byte* arr, int nCount, asn1SccSint asn1SizeMin, asn1SccSint asn1SizeMax) {
	flag ret= (nCount >= asn1SizeMin && nCount <= asn1SizeMax);

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
}

flag BitStream_DecodeOctetString(BitStream* pBitStrm, byte* arr, int* nCount, asn1SccSint asn1SizeMin, asn1SccSint asn1SizeMax) {
	flag ret = TRUE;
	if (asn1SizeMax < 65536) {
		asn1SccSint nCountL;
		if (asn1SizeMin < asn1SizeMax) {
			ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, &nCountL, asn1SizeMin, asn1SizeMax);
		}
		else {
			ret = TRUE;
			nCountL = asn1SizeMin;
		}
		*nCount = (int)nCountL;
		ret = ret && (nCountL >= asn1SizeMin && nCountL <= asn1SizeMax);
		if (ret) {
			BitStream_DecodeOctetString_no_length(pBitStrm, arr, *nCount);
		}
	}
	else {
		ret = BitStream_DecodeOctetString_fragmentation(pBitStrm, arr, nCount, asn1SizeMax);
	}
	return ret;
}






#ifdef ASN1SCC_STREAMING
void bitstream_fetch_data_if_required(BitStream* pStrm) {
	if (pStrm->currentByte == pStrm->count && pStrm->fetchDataPrm != NULL) {
		fetchData(pStrm, pStrm->fetchDataPrm);
		pStrm->currentByte = 0;
	}
}


void bitstream_push_data_if_required(BitStream* pStrm) {
	if (pStrm->currentByte == pStrm->count && pStrm->pushDataPrm != NULL) {
		pushData(pStrm, pStrm->pushDataPrm);
		pStrm->currentByte = 0;
	}
}
#endif



