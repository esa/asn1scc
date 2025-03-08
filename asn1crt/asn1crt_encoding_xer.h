#ifndef ASN1SCC_ASN1CRT_ENCODING_XER_H_
#define ASN1SCC_ASN1CRT_ENCODING_XER_H_

#include "asn1crt_encoding.h"

#ifdef  __cplusplus
extern "C" {
#endif

void Xer_EncodeXmlHeader(ByteStream* pByteStrm, const char* xmlHeader);
flag Xer_EncodeComment(ByteStream* pByteStrm, const char* comment, int *pErrCode);

flag Xer_EncodeNull(ByteStream* pByteStrm, const char* elementTag, NullType value, int *pErrCode, int level);
flag Xer_EncodeInteger(ByteStream* pByteStrm, const char* elementTag, asn1SccSint value, int *pErrCode, int level);
flag Xer_EncodePosInteger(ByteStream* pByteStrm, const char* elementTag, asn1SccUint value, int *pErrCode, int level);
flag Xer_EncodeBoolean(ByteStream* pByteStrm, const char* elementTag, flag value, int *pErrCode, int level);
flag Xer_EncodeEnumerated(ByteStream* pByteStrm, const char* elementTag, const char* value, int *pErrCode, int level);
flag Xer_EncodeReal(ByteStream* pByteStrm, const char* elementTag, asn1Real value, int *pErrCode, int level);
flag Xer_EncodeString(ByteStream* pByteStrm, const char* elementTag, const char* value, int *pErrCode, int level);
flag Xer_EncodeOctetString(ByteStream* pByteStrm, const char* elementTag, const byte value[], int nCount, int *pErrCode, int level);
flag Xer_EncodeBitString(ByteStream* pByteStrm, const char* elementTag, const byte value[], int nCount, int *pErrCode, int level);
flag Xer_EncodeObjectIdentifier(ByteStream* pByteStrm, const char* elementTag, const Asn1ObjectIdentifier *pVal, int *pErrCode, int level);

flag Xer_DecodeNull(ByteStream* pByteStrm, const char* elementTag, NullType* value, int *pErrCode);
flag Xer_DecodeInteger(ByteStream* pByteStrm, const char* elementTag, asn1SccSint* value, int *pErrCode);
flag Xer_DecodePosInteger(ByteStream* pByteStrm, const char* elementTag, asn1SccUint* value, int *pErrCode);
flag Xer_DecodeBoolean(ByteStream* pByteStrm, const char* elementTag, flag* value, int *pErrCode);
flag Xer_DecodeEnumerated(ByteStream* pByteStrm, const char* elementTag, char* value, int *pErrCode);
flag Xer_DecodeReal(ByteStream* pByteStrm, const char* elementTag, asn1Real* value, int *pErrCode);
flag Xer_DecodeString(ByteStream* pByteStrm, const char* elementTag, char* value, int *pErrCode);
flag Xer_DecodeOctetString(ByteStream* pByteStrm, const char* elementTag, byte value[], int bufferMaxSize, int* nCount, int *pErrCode);
flag Xer_DecodeBitString(ByteStream* pByteStrm, const char* elementTag, byte value[], int bufferMaxSize, int* nCount, int *pErrCode);
flag Xer_DecodeObjectIdentifier(ByteStream* pByteStrm, const char* elementTag, Asn1ObjectIdentifier *pVal, int *pErrCode);

flag Xer_EncodeComplexElementStart(ByteStream* pByteStrm, const char* elementTag, XmlAttributeArray* pAttrs, int *pErrCode, int level);
flag Xer_EncodeComplexElementEnd(ByteStream* pByteStrm, const char* elementTag, int *pErrCode, int level);
flag Xer_DecodeComplexElementStart(ByteStream* pByteStrm, const char* elementTag, XmlAttributeArray* pAttrs, int *pErrCode);
flag Xer_DecodeComplexElementEnd(ByteStream* pByteStrm, const char* elementTag, int *pErrCode);
flag Xer_NextEndElementIs(ByteStream* pByteStrm, const char* elementTag);
flag Xer_NextStartElementIs(ByteStream* pByteStrm, const char* elementTag);
flag Xer_LA_NextElementTag(ByteStream* pByteStrm, char* elementTag);
flag LoadXmlFile(const char* fileName, ByteStream* pStrm, int* nBytesLoaded);




#ifdef  __cplusplus
}
#endif


#endif
