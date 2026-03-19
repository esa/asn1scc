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


/*ACN Integer functions*/
void Acn_Enc_Int_PositiveInteger_ConstSize(BitStream* pBitStrm, asn1SccUint intVal, int encodedSizeInBits);
void Acn_Enc_Int_PositiveInteger_ConstSize_8(BitStream* pBitStrm, asn1SccUint intVal);
void Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32(BitStream* pBitStrm, asn1SccUint intVal);

void Acn_Enc_Int_TwosComplement_ConstSize(BitStream* pBitStrm, asn1SccSint intVal, int encodedSizeInBits);
void Acn_Enc_Int_TwosComplement_ConstSize_big_endian_32(BitStream* pBitStrm, asn1SccSint intVal);





/*ACN Decode Integer functions*/
flag Acn_Dec_Int_PositiveInteger_ConstSize(BitStream* pBitStrm, asn1SccUint* pIntVal, int encodedSizeInBits);
flag Acn_Dec_Int_PositiveInteger_ConstSize_8(BitStream* pBitStrm, asn1SccUint* pIntVal);
flag Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32(BitStream* pBitStrm, asn1SccUint* pIntVal);

flag Acn_Dec_Int_TwosComplement_ConstSize(BitStream* pBitStrm, asn1SccSint* pIntVal, int encodedSizeInBits);
flag Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32(BitStream* pBitStrm, asn1SccSint* pIntVal);

/*encoding ends when 'F' is reached*/



/*flag Acn_Dec_Int_ASCII_NullTerminated_FormattedInteger(BitStream* pBitStrm, const char* format, asn1SccSint* pIntVal);*/











































































/* Boolean Decode */

flag BitStream_ReadBitPattern(BitStream* pBitStrm, const byte* patternToRead, int nBitsToRead, flag* pBoolValue);

/* NULL Type functions*/

/*Real encoding functions*/



/*String functions*/



/* Length Determinant functions*/





#ifdef  __cplusplus
}
#endif

#endif
