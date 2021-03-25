#ifndef ASN1SCC_ASN1CRT_ENCODING_ACN_H_
#define ASN1SCC_ASN1CRT_ENCODING_ACN_H_

#include "asn1crt_encoding_uper.h"

#ifdef  __cplusplus
extern "C" {
#endif


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

void Acn_AlignToNextByte(BitStream* pBitStrm, flag bEncode);
void Acn_AlignToNextWord(BitStream* pBitStrm, flag bEncode);
void Acn_AlignToNextDWord(BitStream* pBitStrm, flag bEncode);

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
void Acn_Enc_SInt_ASCII_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccSint intVal, const byte null_characters[], size_t null_characters_size); /*encoding ends when null_character is reached*/

void Acn_Enc_UInt_ASCII_ConstSize(BitStream* pBitStrm, asn1SccUint intVal, int encodedSizeInBytes);
void Acn_Enc_UInt_ASCII_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint intVal);
void Acn_Enc_UInt_ASCII_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccUint intVal, const byte null_characters[], size_t null_characters_size); /*encoding ends when null_character is reached*/


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
flag Acn_Dec_SInt_ASCII_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccSint* pIntVal, const byte null_characters[], size_t null_characters_size);

flag Acn_Dec_UInt_ASCII_ConstSize(BitStream* pBitStrm, asn1SccUint* pIntVal, int encodedSizeInBytes);
flag Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded(BitStream* pBitStrm, asn1SccUint* pIntVal);
flag Acn_Dec_UInt_ASCII_VarSize_NullTerminated(BitStream* pBitStrm, asn1SccUint* pIntVal, const byte null_characters[], size_t null_characters_size);

/*flag Acn_Dec_Int_ASCII_NullTerminated_FormattedInteger(BitStream* pBitStrm, const char* format, asn1SccSint* pIntVal);*/


/* Boolean Decode */

flag BitStream_ReadBitPattern(BitStream* pBitStrm, const byte* patternToRead, int nBitsToRead, flag* pBoolValue);

/* NULL Type functions*/
flag BitStream_ReadBitPattern_ignore_value(BitStream* pBitStrm, int nBitsToRead);

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
void Acn_Enc_String_Ascii_Null_Teminated_mult           (BitStream* pBitStrm, asn1SccSint max, const byte null_character[], size_t null_character_size, const char* strVal);
void Acn_Enc_String_Ascii_External_Field_Determinant    (BitStream* pBitStrm, asn1SccSint max, const char* strVal); 
void Acn_Enc_String_Ascii_Internal_Field_Determinant    (BitStream* pBitStrm, asn1SccSint max, asn1SccSint min, const char* strVal); 
void Acn_Enc_String_CharIndex_FixSize                   (BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, const char* strVal); 
void Acn_Enc_String_CharIndex_External_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, const char* strVal); 
void Acn_Enc_String_CharIndex_Internal_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, byte allowedCharSet[], int charSetSize, asn1SccSint min, const char* strVal); 
void Acn_Enc_IA5String_CharIndex_External_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, const char* strVal);
void Acn_Enc_IA5String_CharIndex_Internal_Field_Determinant(BitStream* pBitStrm, asn1SccSint max, asn1SccSint min, const char* strVal);

flag Acn_Dec_String_Ascii_FixSize                       (BitStream* pBitStrm, asn1SccSint max, char* strVal); 
flag Acn_Dec_String_Ascii_Null_Teminated                (BitStream* pBitStrm, asn1SccSint max, char null_character, char* strVal); 
flag Acn_Dec_String_Ascii_Null_Teminated_mult           (BitStream* pBitStrm, asn1SccSint max, const byte null_character[], size_t null_character_size, char* strVal);
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



asn1SccSint milbus_encode(asn1SccSint val);
asn1SccSint milbus_decode(asn1SccSint val);


#ifdef  __cplusplus
}
#endif

#endif
