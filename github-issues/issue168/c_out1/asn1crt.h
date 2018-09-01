#ifndef ASN1SCC_ASN1CRT_H_
#define ASN1SCC_ASN1CRT_H_

#if (!defined(_MSC_VER) || _MSC_VER >= 1800)
#  ifndef SWIG
#    include <stdbool.h>
#  endif
#else
typedef unsigned char bool;
#define true 1
#define false 0
#endif

#ifdef  __cplusplus
extern "C" {
#endif

#ifndef NULL
#define NULL	0
#endif

#ifndef TRUE
#define TRUE	true
#endif

#ifndef FALSE
#define FALSE	false
#endif

#ifndef WORD_SIZE
#define WORD_SIZE	8
#endif

#ifndef FP_WORD_SIZE
#define FP_WORD_SIZE	8
#endif



typedef float asn1Real32;
typedef double asn1Real64;



typedef unsigned char byte;

typedef int asn1SccSint32;
typedef unsigned int asn1SccUint32;

typedef long long asn1SccSint64;
typedef unsigned long long asn1SccUint64;

#if WORD_SIZE==8
typedef asn1SccUint64 asn1SccUint;
typedef asn1SccSint64 asn1SccSint;
#else
typedef asn1SccUint32 asn1SccUint;
typedef asn1SccSint32 asn1SccSint;
#endif

#if FP_WORD_SIZE==8
typedef asn1Real64 asn1Real;
#else
typedef asn1Real32 asn1Real;
#endif


#ifdef _MSC_VER
#  ifndef INFINITY
#    define INFINITY (DBL_MAX+DBL_MAX)
#  endif
#  ifndef NAN
#    define NAN (INFINITY-INFINITY)
#  endif
#endif

typedef bool flag;

typedef char NullType;

typedef struct {
	byte* buf;
	long count;
	long currentByte;
	/* Next available bit for writting. Possible vallues 0..7, 0 is most significant bit of current byte*/
	int currentBit; 
} BitStream;

typedef struct {
	byte* buf;
	long count;
	long currentByte;
	flag EncodeWhiteSpace;
} ByteStream;

typedef struct {
	int TokenID;
	char Value[100];
} Token;

typedef struct {
	char Name[50];
	char Value[100];
} XmlAttribute;

typedef struct {
	XmlAttribute attrs[20];
	int nCount;
} XmlAttributeArray;


#define ERR_INSUFFICIENT_DATA	101
#define ERR_INCORRECT_PER_STREAM	102
#define ERR_INVALID_CHOICE_ALTERNATIVE	103
#define ERR_INVALID_ENUM_VALUE	104
#define ERR_INVALID_XML_FILE	200
#define ERR_INVALID_BER_FILE	201
#define ERR_BER_LENGTH_MISMATCH	202

/* Bit strean functions */

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





void BitStream_EncodeReal(BitStream* pBitStrm, asn1Real v);
flag BitStream_DecodeReal(BitStream* pBitStrm, asn1Real* v);



void CalculateMantissaAndExponent(asn1Real d, int* exp, asn1SccUint64* mantissa);
asn1Real GetDoubleByMantissaAndExp(asn1SccUint mantissa, int exp);

int GetNumberOfBitsForNonNegativeInteger(asn1SccUint v);

int GetCharIndex(char ch, byte allowedCharSet[], int setLen);


/* 
                                                                                                                                                       
       db         ,ad8888ba,   888b      88           88888888888                                             88                                       
      d88b       d8"'    `"8b  8888b     88           88                                               ,d     ""                                       
     d8'`8b     d8'            88 `8b    88           88                                               88                                              
    d8'  `8b    88             88  `8b   88           88aaaaa  88       88  8b,dPPYba,    ,adPPYba,  MM88MMM  88   ,adPPYba,   8b,dPPYba,   ,adPPYba,  
   d8YaaaaY8b   88             88   `8b  88           88"""""  88       88  88P'   `"8a  a8"     ""    88     88  a8"     "8a  88P'   `"8a  I8[    ""  
  d8""""""""8b  Y8,            88    `8b 88           88       88       88  88       88  8b            88     88  8b       d8  88       88   `"Y8ba,   
 d8'        `8b  Y8a.    .a8P  88     `8888           88       "8a,   ,a88  88       88  "8a,   ,aa    88,    88  "8a,   ,a8"  88       88  aa    ]8I  
d8'          `8b  `"Y8888Y"'   88      `888           88        `"YbbdP'Y8  88       88   `"Ybbd8"'    "Y888  88   `"YbbdP"'   88       88  `"YbbdP"
*/

void Acn_AlignToNextByte(BitStream* pBitStrm);
void Acn_AlignToNextWord(BitStream* pBitStrm);
void Acn_AlignToNextDWord(BitStream* pBitStrm);

/*ACN Integer functions*/
void Acn_Enc_Int_PositiveInteger_ConstSize(BitStream* pBitStrm, asn1SccUint intVal, int encodedSizeInBits);
void Acn_Enc_Int_PositiveInteger_ConstSize_8(BitStream* pBitStrm, asn1SccUint intVal);
void Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16(BitStream* pBitStrm, asn1SccUint intVal);
void Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32(BitStream* pBitStrm, asn1SccUint intVal);
void Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64(BitStream* pBitStrm, asn1SccUint intVal);
void Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16(BitStream* pBitStrm, asn1SccUint intVal);
void Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32(BitStream* pBitStrm, asn1SccUint intVal);
void Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64(BitStream* pBitStrm, asn1SccUint intVal);
void Acn_Enc_Int_PositiveInteger_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint intVal);

void Acn_Enc_Int_TwosComplement_ConstSize(BitStream* pBitStrm, asn1SccSint intVal, int encodedSizeInBits);
void Acn_Enc_Int_TwosComplement_ConstSize_8(BitStream* pBitStrm, asn1SccSint intVal);
void Acn_Enc_Int_TwosComplement_ConstSize_big_endian_16(BitStream* pBitStrm, asn1SccSint intVal);
void Acn_Enc_Int_TwosComplement_ConstSize_big_endian_32(BitStream* pBitStrm, asn1SccSint intVal);
void Acn_Enc_Int_TwosComplement_ConstSize_big_endian_64(BitStream* pBitStrm, asn1SccSint intVal);
void Acn_Enc_Int_TwosComplement_ConstSize_little_endian_16(BitStream* pBitStrm, asn1SccSint intVal);
void Acn_Enc_Int_TwosComplement_ConstSize_little_endian_32(BitStream* pBitStrm, asn1SccSint intVal);
void Acn_Enc_Int_TwosComplement_ConstSize_little_endian_64(BitStream* pBitStrm, asn1SccSint intVal);
void Acn_Enc_Int_TwosComplement_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccSint intVal);

void Acn_Enc_Int_BCD_ConstSize(BitStream* pBitStrm, asn1SccUint intVal, int encodedSizeInNibbles);
void Acn_Enc_Int_BCD_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint intVal);
void Acn_Enc_Int_BCD_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccUint intVal); /*encoding ends when 'F' is reached*/

void Acn_Enc_SInt_ASCII_ConstSize(BitStream* pBitStrm, asn1SccSint intVal, int encodedSizeInBytes);
void Acn_Enc_SInt_ASCII_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccSint intVal);
void Acn_Enc_SInt_ASCII_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccSint intVal, byte null_character); /*encoding ends when null_character is reached*/

void Acn_Enc_UInt_ASCII_ConstSize(BitStream* pBitStrm, asn1SccUint intVal, int encodedSizeInBytes);
void Acn_Enc_UInt_ASCII_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint intVal);
void Acn_Enc_UInt_ASCII_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccUint intVal, byte null_character); /*encoding ends when null_character is reached*/


/*ACN Decode Integer functions*/
flag Acn_Dec_Int_PositiveInteger_ConstSize(BitStream* pBitStrm, asn1SccUint* pIntVal, int encodedSizeInBits);
flag Acn_Dec_Int_PositiveInteger_ConstSize_8(BitStream* pBitStrm, asn1SccUint* pIntVal);
flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16(BitStream* pBitStrm, asn1SccUint* pIntVal);
flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32(BitStream* pBitStrm, asn1SccUint* pIntVal);
flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64(BitStream* pBitStrm, asn1SccUint* pIntVal);
flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16(BitStream* pBitStrm, asn1SccUint* pIntVal);
flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32(BitStream* pBitStrm, asn1SccUint* pIntVal);
flag Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64(BitStream* pBitStrm, asn1SccUint* pIntVal);
flag Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint* pIntVal);

flag Acn_Dec_Int_TwosComplement_ConstSize(BitStream* pBitStrm, asn1SccSint* pIntVal, int encodedSizeInBits);
flag Acn_Dec_Int_TwosComplement_ConstSize_8(BitStream* pBitStrm, asn1SccSint* pIntVal);
flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16(BitStream* pBitStrm, asn1SccSint* pIntVal);
flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32(BitStream* pBitStrm, asn1SccSint* pIntVal);
flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64(BitStream* pBitStrm, asn1SccSint* pIntVal);
flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16(BitStream* pBitStrm, asn1SccSint* pIntVal);
flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32(BitStream* pBitStrm, asn1SccSint* pIntVal);
flag Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64(BitStream* pBitStrm, asn1SccSint* pIntVal);
flag Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccSint* pIntVal);

flag Acn_Dec_Int_BCD_ConstSize(BitStream* pBitStrm, asn1SccUint* pIntVal, int encodedSizeInNibbles);
flag Acn_Dec_Int_BCD_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint* pIntVal);
/*encoding ends when 'F' is reached*/
flag Acn_Dec_Int_BCD_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccUint* pIntVal);

flag Acn_Dec_SInt_ASCII_ConstSize(BitStream* pBitStrm, asn1SccSint* pIntVal, int encodedSizeInBytes);
flag Acn_Dec_SInt_ASCII_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccSint* pIntVal);
flag Acn_Dec_SInt_ASCII_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccSint* pIntVal, byte null_character);

flag Acn_Dec_UInt_ASCII_ConstSize(BitStream* pBitStrm, asn1SccUint* pIntVal, int encodedSizeInBytes);
flag Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint* pIntVal);
flag Acn_Dec_UInt_ASCII_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccUint* pIntVal, byte null_character);

/*flag Acn_Dec_Int_ASCII_NullTerminated_FormattedInteger(BitStream* pBitStrm, const char* format, asn1SccSint* pIntVal);*/


/* Boolean Decode */

flag BitStream_ReadBitPattern(BitStream* pBitStrm, const byte* patternToRead, int nBitsToRead, flag* pBoolValue);

/*Real encoding functions*/
void Acn_Enc_Real_IEEE754_32_big_endian(BitStream* pBitStrm, asn1Real realValue);
void Acn_Enc_Real_IEEE754_64_big_endian(BitStream* pBitStrm, asn1Real realValue);
void Acn_Enc_Real_IEEE754_32_little_endian(BitStream* pBitStrm, asn1Real realValue);
void Acn_Enc_Real_IEEE754_64_little_endian(BitStream* pBitStrm, asn1Real realValue);

flag Acn_Dec_Real_IEEE754_32_big_endian(BitStream* pBitStrm, asn1Real* pRealValue);
flag Acn_Dec_Real_IEEE754_64_big_endian(BitStream* pBitStrm, asn1Real* pRealValue);
flag Acn_Dec_Real_IEEE754_32_little_endian(BitStream* pBitStrm, asn1Real* pRealValue);
flag Acn_Dec_Real_IEEE754_64_little_endian(BitStream* pBitStrm, asn1Real* pRealValue);

/*String functions*/
void Acn_Enc_String_Ascii_FixSize                       (BitStream* pBitStrm, asn1SccSint max, const char* strVal); 
void Acn_Enc_String_Ascii_Null_Teminated                (BitStream* pBitStrm, asn1SccSint max, char null_character, const char* strVal); 
void Acn_Enc_String_Ascii_External_Field_Determinant    (BitStream* pBitStrm, asn1SccSint max, const char* strVal); 
void Acn_Enc_String_Ascii_Internal_Field_Determinant    (BitStream* pBitStrm, asn1SccSint max, asn1SccSint min, const char* strVal); 
void Acn_Enc_String_CharIndex_FixSize                   (BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, const char* strVal); 
void Acn_Enc_String_CharIndex_External_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, const char* strVal); 
void Acn_Enc_String_CharIndex_Internal_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, asn1SccSint min, const char* strVal); 
void Acn_Enc_IA5String_CharIndex_External_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, const char* strVal);
void Acn_Enc_IA5String_CharIndex_Internal_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, asn1SccSint min, const char* strVal);

flag Acn_Dec_String_Ascii_FixSize                       (BitStream* pBitStrm, asn1SccSint max, char* strVal); 
flag Acn_Dec_String_Ascii_Null_Teminated                (BitStream* pBitStrm, asn1SccSint max, char null_character, char* strVal); 
flag Acn_Dec_String_Ascii_External_Field_Determinant    (BitStream* pBitStrm, asn1SccSint max, asn1SccSint extSizeDeterminatFld, char* strVal); 
flag Acn_Dec_String_Ascii_Internal_Field_Determinant    (BitStream* pBitStrm, asn1SccSint max, asn1SccSint min, char* strVal); 
flag Acn_Dec_String_CharIndex_FixSize                   (BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, char* strVal); 
flag Acn_Dec_String_CharIndex_External_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, asn1SccSint extSizeDeterminatFld, char* strVal); 
flag Acn_Dec_String_CharIndex_Internal_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, asn1SccSint min, char* strVal); 
flag Acn_Dec_IA5String_CharIndex_External_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, asn1SccSint extSizeDeterminatFld, char* strVal);
flag Acn_Dec_IA5String_CharIndex_Internal_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, asn1SccSint min, char* strVal);




/* Length Determinant functions*/
void Acn_Enc_Length(BitStream* pBitStrm, asn1SccUint lengthValue, int lengthSizeInBits);

flag Acn_Dec_Length(BitStream* pBitStrm, asn1SccUint* pLengthValue, int lengthSizeInBits);



void BitStream_AppendNBitZero(BitStream* pBitStrm, int nbits);
void BitStream_EncodeNonNegativeInteger(BitStream* pBitStrm, asn1SccUint v);
void BitStream_AppendNBitOne(BitStream* pBitStrm, int nbits);
void BitStream_EncodeNonNegativeIntegerNeg(BitStream* pBitStrm, asn1SccUint v, flag negate);
flag BitStream_DecodeNonNegativeInteger(BitStream* pBitStrm, asn1SccUint* v, int nBits);
flag BitStream_ReadPartialByte(BitStream* pBitStrm, byte *v, byte nbits);
void BitStream_AppendPartialByte(BitStream* pBitStrm, byte v, byte nbits, flag negate);





void Xer_EncodeXmlHeader(ByteStream* pByteStrm, const char* xmlHeader);
flag Xer_EncodeComment(ByteStream* pByteStrm, const char* comment, int *pErrCode);

flag Xer_EncodeInteger(ByteStream* pByteStrm, const char* elementTag, asn1SccSint value, int *pErrCode, int level);
flag Xer_EncodePosInteger(ByteStream* pByteStrm, const char* elementTag, asn1SccUint value, int *pErrCode, int level);
flag Xer_EncodeBoolean(ByteStream* pByteStrm, const char* elementTag, flag value, int *pErrCode, int level);
flag Xer_EncodeEnumerated(ByteStream* pByteStrm, const char* elementTag, const char* value, int *pErrCode, int level);
flag Xer_EncodeReal(ByteStream* pByteStrm, const char* elementTag, asn1Real value, int *pErrCode, int level);
flag Xer_EncodeString(ByteStream* pByteStrm, const char* elementTag, const char* value, int *pErrCode, int level);
flag Xer_EncodeOctetString(ByteStream* pByteStrm, const char* elementTag, const byte value[], int nCount, int *pErrCode, int level);
flag Xer_EncodeBitString(ByteStream* pByteStrm, const char* elementTag, const byte value[], int nCount, int *pErrCode, int level);



flag Xer_DecodeInteger(ByteStream* pByteStrm, const char* elementTag, asn1SccSint* value, int *pErrCode);
flag Xer_DecodePosInteger(ByteStream* pByteStrm, const char* elementTag, asn1SccUint* value, int *pErrCode);
flag Xer_DecodeBoolean(ByteStream* pByteStrm, const char* elementTag, flag* value, int *pErrCode);
flag Xer_DecodeEnumerated(ByteStream* pByteStrm, const char* elementTag, char* value, int *pErrCode);
flag Xer_DecodeReal(ByteStream* pByteStrm, const char* elementTag, asn1Real* value, int *pErrCode);
flag Xer_DecodeString(ByteStream* pByteStrm, const char* elementTag, char* value, int *pErrCode);
flag Xer_DecodeOctetString(ByteStream* pByteStrm, const char* elementTag, byte value[], int bufferMaxSize, int* nCount, int *pErrCode);
flag Xer_DecodeBitString(ByteStream* pByteStrm, const char* elementTag, byte value[], int bufferMaxSize, int* nCount, int *pErrCode);
flag Xer_EncodeComplexElementStart(ByteStream* pByteStrm, const char* elementTag, XmlAttributeArray* pAttrs, int *pErrCode, int level);
flag Xer_EncodeComplexElementEnd(ByteStream* pByteStrm, const char* elementTag, int *pErrCode, int level);
flag Xer_DecodeComplexElementStart(ByteStream* pByteStrm, const char* elementTag, XmlAttributeArray* pAttrs, int *pErrCode);
flag Xer_DecodeComplexElementEnd(ByteStream* pByteStrm, const char* elementTag, int *pErrCode);
flag Xer_NextEndElementIs(ByteStream* pByteStrm, const char* elementTag);
flag Xer_NextStartElementIs(ByteStream* pByteStrm, const char* elementTag);
flag Xer_LA_NextElementTag(ByteStream* pByteStrm, char* elementTag);
flag LoadXmlFile(const char* fileName, ByteStream* pStrm, int* nBytesLoaded);




typedef asn1SccUint BerTag;




flag BerEncodeTag(ByteStream* pByteStrm, BerTag tag, int *pErrCode);
flag BerDecodeTag(ByteStream* pByteStrm, BerTag tag, int *pErrCode);

flag BerEncodeLengthStart(ByteStream* pByteStrm, int *pErrCode);
flag BerEncodeLengthEnd(ByteStream* pByteStrm, int *pErrCode);

flag BerDecodeLength(ByteStream* pByteStrm, int* value, int *pErrCode);
flag BerDecodeTwoZeroes(ByteStream* pByteStrm, int *pErrCode);


flag BerEncodeInteger(ByteStream* pByteStrm, BerTag tag, asn1SccSint value, int *pErrCode);
flag BerDecodeInteger(ByteStream* pByteStrm, BerTag tag, asn1SccSint *value, int *pErrCode);

flag BerEncodeBoolean(ByteStream* pByteStrm, BerTag tag, flag value, int *pErrCode);
flag BerDecodeBoolean(ByteStream* pByteStrm, BerTag tag, flag *value, int *pErrCode);

flag BerEncodeReal(ByteStream* pByteStrm, BerTag tag, asn1Real value, int *pErrCode);
flag BerDecodeReal(ByteStream* pByteStrm, BerTag tag, asn1Real *value, int *pErrCode);

flag BerEncodeIA5String(ByteStream* pByteStrm, BerTag tag, const char* value, int length, int *pErrCode);
flag BerDecodeIA5String(ByteStream* pByteStrm, BerTag tag, char* value, int maxLength, int *pErrCode);

flag BerEncodeNull(ByteStream* pByteStrm, BerTag tag, int *pErrCode);
flag BerDecodeNull(ByteStream* pByteStrm, BerTag tag, int *pErrCode);

flag BerEncodeBitString(ByteStream* pByteStrm, BerTag tag, const byte* value, int bitCount, int *pErrCode);
flag BerDecodeBitString(ByteStream* pByteStrm, BerTag tag, byte* value, int *bitCount, int maxBitCount, int *pErrCode);

flag BerEncodeOctetString(ByteStream* pByteStrm, BerTag tag, const byte* value, int octCount, int *pErrCode);
flag BerDecodeOctetString(ByteStream* pByteStrm, BerTag tag, byte* value, int *octCount, int maxOctCount, int *pErrCode);


flag NextTagMatches(ByteStream* pByteStrm, BerTag tag);

flag LA_Next_Two_Bytes_00(ByteStream* pByteStrm);

asn1SccSint milbus_encode(asn1SccSint val);
asn1SccSint milbus_decode(asn1SccSint val);

#if WORD_SIZE==8
extern const asn1SccUint64 ber_aux[];
#else
extern const asn1SccUint32 ber_aux[];
#endif


asn1SccUint int2uint(asn1SccSint v);
asn1SccSint uint2int(asn1SccUint v, int uintSizeInBytes);


#define CHECK_BIT_STREAM(pBitStrm)	assert((pBitStrm)->currentByte*8+(pBitStrm)->currentBit<=(pBitStrm)->count*8)

#ifdef _MSC_VER
#pragma warning( disable : 4127)
#endif

#define ASSERT_OR_RETURN_FALSE(_Expression) do { assert(_Expression); if (!(_Expression)) return FALSE;} while(0)

#ifdef  __cplusplus
}
#endif


#endif
