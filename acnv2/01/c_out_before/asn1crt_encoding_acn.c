#include <string.h>
#include <assert.h>

#include "asn1crt_encoding_acn.h"









/*ACN Integer functions*/






































































































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




































































































































































































































