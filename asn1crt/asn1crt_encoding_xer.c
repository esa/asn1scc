#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <float.h>
#include <ctype.h>
#include <stdlib.h>
#include <inttypes.h>

#include "asn1crt_encoding_xer.h"

char* Int2String(asn1SccSint v) {
	static char tmp[256];
	snprintf(tmp, sizeof(tmp), "%" ASN1SCC_PRId, v);
	return tmp;
}

char* UInt2String(asn1SccUint v) {
	static char tmp[256];
	snprintf(tmp, sizeof(tmp), "%" ASN1SCC_PRIu, v);
	return tmp;
}

char* Double2String(double v) {
	if (fabs(v) < 1e-17)
		return "0";

	static char tmp[256];
	int exponent = 0;

	while (fabs(v) >= 10) {
		v /= 10;
		exponent++;
	}

	while (fabs(v) < 1) {
		v *= 10;
		exponent--;
	}

	if (fabs(fabs(v) - llabs((asn1SccSint)v)) < 1e-17) {
		snprintf(tmp, sizeof(tmp), "%.1fE%d", v, exponent);
	}
	else {
		snprintf(tmp, sizeof(tmp), "%.17gE%d", v, exponent);
	}

	return tmp;
}

flag GetNextChar(ByteStream* pStrm, char* c) {
	if (pStrm->currentByte >= pStrm->count)
		return FALSE;
	*c = (char)pStrm->buf[pStrm->currentByte];
	pStrm->currentByte++;
	return TRUE;
}

void PushBackChar(ByteStream* pStrm)
{
	pStrm->currentByte--;
	assert(pStrm->currentByte >= 0);
}

flag ByteStream_PutSpace(ByteStream* pStrm, int level)
{
	int i;
	int len = 2 * level;

	if (!pStrm->EncodeWhiteSpace)
		return TRUE;

	if (level<0)
		return TRUE;
	if (pStrm->currentByte + len >= pStrm->count)
		return FALSE;

	for (i = 0; i< len; i++) {
		pStrm->buf[pStrm->currentByte] = ' ';
		pStrm->currentByte++;
	}
	return TRUE;
}

flag ByteStream_PutNL(ByteStream* pStrm)
{
	if (!pStrm->EncodeWhiteSpace)
		return TRUE;

	if (pStrm->currentByte >= pStrm->count)
		return FALSE;

	pStrm->buf[pStrm->currentByte] = '\n';
	pStrm->currentByte++;
	return TRUE;
}

flag ByteStream_AppendString(ByteStream* pStrm, const char* v)
{
	int len = (int)strlen(v);
	if (pStrm->currentByte + len >= pStrm->count)
		return FALSE;

	strcat((char*)&pStrm->buf[pStrm->currentByte], v);
	pStrm->currentByte += len;
	return TRUE;
}



#define WORD_ID 1000

flag isPartOfID(char c) {
	if (isalpha(c))
		return TRUE;
	if (isdigit(c))
		return TRUE;
	if (c == '.')
		return TRUE;
	if (c == '+')
		return TRUE;
	if (c == '-')
		return TRUE;
	if (c == '_')
		return TRUE;

	return FALSE;
}

Token NT(ByteStream* pByteStrm) {
	Token ret;
	char spChrs[] = { '<', '>', '/', '=', '"', 0 };
	char *tmp;
	memset(&ret, 0x0, sizeof(Token));

	while (isspace(pByteStrm->buf[pByteStrm->currentByte]))
		pByteStrm->currentByte++;

	if ((pByteStrm->buf[pByteStrm->currentByte] == '<') && (pByteStrm->buf[pByteStrm->currentByte + 1] == '?')) {
		pByteStrm->currentByte++;
		while (!((pByteStrm->buf[pByteStrm->currentByte - 1] == '?') && (pByteStrm->buf[pByteStrm->currentByte] == '>')))
			pByteStrm->currentByte++;
		pByteStrm->currentByte++;
		while (isspace(pByteStrm->buf[pByteStrm->currentByte]))
			pByteStrm->currentByte++;
	}

	if ((pByteStrm->buf[pByteStrm->currentByte] == '<') && (pByteStrm->buf[pByteStrm->currentByte + 1] == '!') && (pByteStrm->buf[pByteStrm->currentByte + 2] == '-') && (pByteStrm->buf[pByteStrm->currentByte + 3] == '-')) {
		pByteStrm->currentByte++;
		pByteStrm->currentByte++;
		while (!((pByteStrm->buf[pByteStrm->currentByte - 2] == '-') && (pByteStrm->buf[pByteStrm->currentByte - 1] == '-') && (pByteStrm->buf[pByteStrm->currentByte] == '>')))
			pByteStrm->currentByte++;
		pByteStrm->currentByte++;
		while (isspace(pByteStrm->buf[pByteStrm->currentByte]))
			pByteStrm->currentByte++;
	}

	//if (pByteStrm->buf[pByteStrm->currentByte] =
	tmp = strchr(spChrs, pByteStrm->buf[pByteStrm->currentByte]);
	if (tmp != NULL) {
		ret.TokenID = pByteStrm->buf[pByteStrm->currentByte];
		ret.Value[0] = (char)pByteStrm->buf[pByteStrm->currentByte];
		pByteStrm->currentByte++;
		return ret;
	}

	tmp = &ret.Value[0];
	while (isPartOfID((char)pByteStrm->buf[pByteStrm->currentByte])) {
		ret.TokenID = WORD_ID;
		*tmp = (char)pByteStrm->buf[pByteStrm->currentByte];
		tmp++;
		pByteStrm->currentByte++;
	}


	return ret;
}

Token LA(ByteStream* pByteStrm) {
	int tmp;
	Token ret;
	tmp = pByteStrm->currentByte;

	ret = NT(pByteStrm);

	pByteStrm->currentByte = tmp;

	return ret;
}




void AddAttribute(XmlAttributeArray* pAttrArray, const char* attr, const char* val)
{
	assert(((unsigned)pAttrArray->nCount) < (sizeof(XmlAttributeArray) - sizeof(int)) / sizeof(XmlAttribute));

	strcpy(pAttrArray->attrs[pAttrArray->nCount].Name, attr);
	strcpy(pAttrArray->attrs[pAttrArray->nCount].Value, val);
	pAttrArray->nCount++;
}

/*
<elementTag>value</elementTag>
<elementTag/>
*/
flag Xer_EncodePrimitiveElement(ByteStream* pByteStrm, const char* elementTag, const char* value, int *pErrCode, int level)
{
	*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
	if (!ByteStream_PutSpace(pByteStrm, level))
		return FALSE;

	if (!ByteStream_AppendString(pByteStrm, "<"))
		return FALSE;
	if (!ByteStream_AppendString(pByteStrm, elementTag))
		return FALSE;
	if (value == NULL || strlen(value) == 0) {
		if (!ByteStream_AppendString(pByteStrm, "/"))
			return FALSE;
		if (!ByteStream_AppendString(pByteStrm, ">"))
			return FALSE;
		if (level >= 0)
			if (!ByteStream_PutNL(pByteStrm))
				return FALSE;
		return TRUE;
	}

	if (!ByteStream_AppendString(pByteStrm, ">"))
		return FALSE;
	if (!ByteStream_AppendString(pByteStrm, value))
		return FALSE;
	if (!ByteStream_AppendString(pByteStrm, "<"))
		return FALSE;
	if (!ByteStream_AppendString(pByteStrm, "/"))
		return FALSE;
	if (!ByteStream_AppendString(pByteStrm, elementTag))
		return FALSE;
	if (!ByteStream_AppendString(pByteStrm, ">"))
		return FALSE;
	if (level >= 0)
		if (!ByteStream_PutNL(pByteStrm))
			return FALSE;
	*pErrCode = 0;
	return TRUE;
}


flag Xer_DecodePrimitiveElement(ByteStream* pByteStrm, const char* elementTag, char* pDecodedValue, int *pErrCode)
{
	Token t;
	char c = 0x0;

	strcpy(pDecodedValue, "");
	*pErrCode = ERR_INVALID_XML_FILE; /* +++ */

	if (NT(pByteStrm).TokenID != '<') {
		*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
		return FALSE;
	}

	t = NT(pByteStrm);
	if ((t.TokenID != WORD_ID) || (strcmp(t.Value, elementTag) != 0)) {
		*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
		return FALSE;
	}
	t = NT(pByteStrm);
	if (t.TokenID == '/') {
		if (NT(pByteStrm).TokenID == '>')
			return TRUE;
		else {
			*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
			return FALSE;
		}
	}
	else {
		if (t.TokenID != '>') {
			*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
			return FALSE;
		}
	}


	/*
	t = NT(pByteStrm);
	strcpy(pDecodedValue, t.Value);
	if (t.TokenID != WORD_ID) {
	*pErrCode = ERR_INVALID_XML_FILE;
	return FALSE;
	}
	*/
	while (c != '<')
	{
		if (!GetNextChar(pByteStrm, &c))
			return FALSE;
		if (c == '<') {
			*pDecodedValue = 0x0;
			pDecodedValue++;
			break;
		}

		*pDecodedValue = c;
		pDecodedValue++;
	}

	PushBackChar(pByteStrm);


	if (NT(pByteStrm).TokenID != '<') {
		*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
		return FALSE;
	}

	if (NT(pByteStrm).TokenID != '/') {
		*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
		return FALSE;
	}

	t = NT(pByteStrm);
	if ((t.TokenID != WORD_ID) || (strcmp(t.Value, elementTag) != 0)) {
		*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
		return FALSE;
	}

	if (NT(pByteStrm).TokenID != '>') {
		*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
		return FALSE;
	}

	return TRUE;
}



/*
attrName="attrValue"
*/


flag Xer_DecodeAttributes(ByteStream* pByteStrm, XmlAttributeArray* pAttrs, int *pErrCode)
{
	Token t1;
	Token t2;

	while (LA(pByteStrm).TokenID != '>') {
		t1 = NT(pByteStrm);
		if (t1.TokenID != WORD_ID) {
			*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
			return FALSE;
		}

		if (NT(pByteStrm).TokenID != '=') {
			*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
			return FALSE;
		}

		if (NT(pByteStrm).TokenID != '"') {
			*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
			return FALSE;
		}

		t2 = NT(pByteStrm);
		if (t2.TokenID != WORD_ID) {
			*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
			return FALSE;
		}

		if (NT(pByteStrm).TokenID != '"') {
			*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
			return FALSE;
		}
		AddAttribute(pAttrs, t1.Value, t2.Value);
	}
	return TRUE;
}


flag Xer_EncodeComplexElementStart(ByteStream* pByteStrm, const char* elementTag, XmlAttributeArray* pAttrs, int *pErrCode, int level)
{
	int i = 0;
	*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
	if (elementTag == NULL || strlen(elementTag) == 0)
		return TRUE;

	if (!ByteStream_PutSpace(pByteStrm, level))
		return FALSE;


	if (!ByteStream_AppendString(pByteStrm, "<"))
		return FALSE;
	if (!ByteStream_AppendString(pByteStrm, elementTag))
		return FALSE;
	if (pAttrs != NULL) {
		for (i = 0; i<pAttrs->nCount; i++) {
			if (!ByteStream_AppendString(pByteStrm, pAttrs->attrs[i].Name))
				return FALSE;
			if (!ByteStream_AppendString(pByteStrm, "="))
				return FALSE;
			if (!ByteStream_AppendString(pByteStrm, "\""))
				return FALSE;
			if (!ByteStream_AppendString(pByteStrm, pAttrs->attrs[i].Value))
				return FALSE;
			if (!ByteStream_AppendString(pByteStrm, "\""))
				return FALSE;
		}
	}

	if (!ByteStream_AppendString(pByteStrm, ">"))
		return FALSE;
	if (level >= 0)
		if (!ByteStream_PutNL(pByteStrm))
			return FALSE;

	return TRUE;
}

flag Xer_DecodeComplexElementStart(ByteStream* pByteStrm, const char* elementTag, XmlAttributeArray* pAttrs, int *pErrCode)
{
	Token t;

	if (elementTag == NULL || strlen(elementTag) == 0)
		return TRUE;

	if (NT(pByteStrm).TokenID != '<') {
		*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
		return FALSE;
	}

	t = NT(pByteStrm);
	if ((t.TokenID != WORD_ID) || (strcmp(t.Value, elementTag) != 0)) {
		*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
		return FALSE;
	}

	if (!Xer_DecodeAttributes(pByteStrm, pAttrs, pErrCode))
		return FALSE;


	t = NT(pByteStrm);
	if (t.TokenID == '/') {
		if (NT(pByteStrm).TokenID == '>') {
			return TRUE;
		}
		else {
			*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
			return FALSE;
		}
	}
	else {
		if (t.TokenID != '>') {
			*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
			return FALSE;
		}
	}

	return TRUE;
}

flag Xer_EncodeComplexElementEnd(ByteStream* pByteStrm, const char* elementTag, int *pErrCode, int level)
{
	*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
	if (elementTag == NULL || strlen(elementTag) == 0)
		return TRUE;
	if (!ByteStream_PutSpace(pByteStrm, level))
		return FALSE;

	if (!ByteStream_AppendString(pByteStrm, "<"))
		return FALSE;
	if (!ByteStream_AppendString(pByteStrm, "/"))
		return FALSE;
	if (!ByteStream_AppendString(pByteStrm, elementTag))
		return FALSE;
	if (!ByteStream_AppendString(pByteStrm, ">"))
		return FALSE;
	*pErrCode = 0;

	if (level >= 0)
		if (!ByteStream_PutNL(pByteStrm))
			return FALSE;
	return TRUE;
}

flag Xer_DecodeComplexElementEnd(ByteStream* pByteStrm, const char* elementTag, int *pErrCode)
{
	Token t;
	
	if (elementTag == NULL || strlen(elementTag) == 0)
        return TRUE;

	if (NT(pByteStrm).TokenID != '<') {
		*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
		return FALSE;
	}

	if (NT(pByteStrm).TokenID != '/') {
		*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
		return FALSE;
	}

	t = NT(pByteStrm);
	if ((t.TokenID != WORD_ID) || (strcmp(t.Value, elementTag) != 0)) {
		*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
		return FALSE;
	}

	if (NT(pByteStrm).TokenID != '>') {
		*pErrCode = ERR_INVALID_XML_FILE; /* +++ */
		return FALSE;
	}

	return TRUE;
}




flag Xer_EncodeNull(ByteStream* pByteStrm, const char* elementTag, NullType value, int *pErrCode, int level)
{
	(void)value;
	return Xer_EncodePrimitiveElement(pByteStrm, elementTag, NULL, pErrCode, level);
}

flag Xer_EncodeInteger(ByteStream* pByteStrm, const char* elementTag, asn1SccSint value, int *pErrCode, int level)
{
	return Xer_EncodePrimitiveElement(pByteStrm, elementTag, Int2String(value), pErrCode, level);
}

flag Xer_EncodePosInteger(ByteStream* pByteStrm, const char* elementTag, asn1SccUint value, int *pErrCode, int level)
{
	return Xer_EncodePrimitiveElement(pByteStrm, elementTag, UInt2String(value), pErrCode, level);
}

flag Xer_EncodeBoolean(ByteStream* pByteStrm, const char* elementTag, flag value, int *pErrCode, int level) {
	if (elementTag == NULL || strlen(elementTag) == 0) {
		if (value)
			return Xer_EncodePrimitiveElement(pByteStrm, "true", "", pErrCode, level);
		else
			return Xer_EncodePrimitiveElement(pByteStrm, "false", "", pErrCode, level);
	}
	else {
		if (value)
			return Xer_EncodePrimitiveElement(pByteStrm, elementTag, "<true/>", pErrCode, level);
		else
			return Xer_EncodePrimitiveElement(pByteStrm, elementTag, "<false/>", pErrCode, level);
	}
}

flag Xer_EncodeEnumerated(ByteStream* pByteStrm, const char* elementTag, const char* value, int *pErrCode, int level)
{
	char tmp[256];
	memset(tmp, 0x0, sizeof(tmp));
	strcat(tmp, "<");
	strcat(tmp, value);
	strcat(tmp, "/>");

	if (elementTag == NULL || strlen(elementTag) == 0)
		return Xer_EncodePrimitiveElement(pByteStrm, value, "", pErrCode, level);

	if (!ByteStream_PutSpace(pByteStrm, level))
		return FALSE;

	if (!Xer_EncodeComplexElementStart(pByteStrm, elementTag, NULL, pErrCode, -1))
		return FALSE;

	if (!Xer_EncodePrimitiveElement(pByteStrm, value, "", pErrCode, -1))
		return FALSE;

	if (!Xer_EncodeComplexElementEnd(pByteStrm, elementTag, pErrCode, -1))
		return FALSE;

	if (!ByteStream_PutNL(pByteStrm))
		return FALSE;


	return TRUE;
}

flag Xer_EncodeReal(ByteStream* pByteStrm, const char* elementTag, asn1Real value, int *pErrCode, int level)
{
	return Xer_EncodePrimitiveElement(pByteStrm, elementTag, Double2String(value), pErrCode, level);
}

flag Xer_EncodeString(ByteStream* pByteStrm, const char* elementTag, const char* value, int *pErrCode, int level)
{
	return Xer_EncodePrimitiveElement(pByteStrm, elementTag, value, pErrCode, level);
}

flag Xer_EncodeOctetString(ByteStream* pByteStrm, const char* elementTag, const byte value[], int nCount, int *pErrCode, int level)
{
	int i;
	*pErrCode = 44444;//++++

	if (!ByteStream_PutSpace(pByteStrm, level))
		return FALSE;

	if (!Xer_EncodeComplexElementStart(pByteStrm, elementTag, NULL, pErrCode, -1))
		return FALSE;

	for (i = 0; i<nCount; i++) {
		char tmp[3];
		snprintf(tmp, sizeof(tmp), "%02X", value[i]);
		if (!ByteStream_AppendString(pByteStrm, tmp))
			return FALSE;

	}
	if (!Xer_EncodeComplexElementEnd(pByteStrm, elementTag, pErrCode, -1))
		return FALSE;

	if (!ByteStream_PutNL(pByteStrm))
		return FALSE;

	return TRUE;
}

flag Xer_EncodeBitString(ByteStream* pByteStrm, const char* elementTag, const byte value[], int nCount, int *pErrCode, int level) {

	int i;
	int curByte;
	int curBit;
	char tmp[2];
	memset(tmp, 0x0, sizeof(tmp));
	*pErrCode = 44444;//++++

	if (!ByteStream_PutSpace(pByteStrm, level))
		return FALSE;

	if (!Xer_EncodeComplexElementStart(pByteStrm, elementTag, NULL, pErrCode, -1))
		return FALSE;

	for (i = 0; i<nCount; i++) {
		curByte = i / 8;
		curBit = 7 - i % 8;

		if (value[curByte] & (1 << curBit))
			tmp[0] = '1';
		else
			tmp[0] = '0';

		if (!ByteStream_AppendString(pByteStrm, tmp))
			return FALSE;
	}

	if (!Xer_EncodeComplexElementEnd(pByteStrm, elementTag, pErrCode, -1))
		return FALSE;

	if (!ByteStream_PutNL(pByteStrm))
		return FALSE;
	return TRUE;
}


flag Xer_EncodeObjectIdentifier(ByteStream* pByteStrm, const char* elementTag, const Asn1ObjectIdentifier *pVal, int *pErrCode, int level)
{
	int i;
	*pErrCode = 44444;//++++

	if (!ByteStream_PutSpace(pByteStrm, level))
		return FALSE;

	if (!Xer_EncodeComplexElementStart(pByteStrm, elementTag, NULL, pErrCode, -1))
		return FALSE;
	if (pVal->nCount > 0) {
		char tmp[40];
		snprintf(tmp, sizeof(tmp), "%" ASN1SCC_PRIu, pVal->values[0]);
		if (!ByteStream_AppendString(pByteStrm, tmp))
			return FALSE;
	}

	for (i = 1; i<pVal->nCount; i++) {
		char tmp[40];
		snprintf(tmp, sizeof(tmp), ".%" ASN1SCC_PRIu, pVal->values[i]);
		if (!ByteStream_AppendString(pByteStrm, tmp))
			return FALSE;

	}
	if (!Xer_EncodeComplexElementEnd(pByteStrm, elementTag, pErrCode, -1))
		return FALSE;

	if (!ByteStream_PutNL(pByteStrm))
		return FALSE;

	return TRUE;
}










flag Xer_DecodeNull(ByteStream* pByteStrm, const char* elementTag, NullType* value, int *pErrCode)
{
	char tmp[256];
	memset(tmp, 0x0, sizeof(tmp));
	if (!Xer_DecodePrimitiveElement(pByteStrm, elementTag, tmp, pErrCode))
		return FALSE;
	*value = 0;
	return TRUE;
}





flag Xer_DecodeInteger(ByteStream* pByteStrm, const char* elementTag, asn1SccSint* value, int *pErrCode)
{
	char tmp[256];
	memset(tmp, 0x0, sizeof(tmp));
	if (!Xer_DecodePrimitiveElement(pByteStrm, elementTag, tmp, pErrCode))
		return FALSE;
	*value = atoll(tmp);
	return TRUE;
}

flag Xer_DecodePosInteger(ByteStream* pByteStrm, const char* elementTag, asn1SccUint* value, int *pErrCode)
{
	char tmp[256];
	memset(tmp, 0x0, sizeof(tmp));
	if (!Xer_DecodePrimitiveElement(pByteStrm, elementTag, tmp, pErrCode))
		return FALSE;
	*value = strtoull(tmp, NULL, 10);
	return TRUE;
}

flag Xer_DecodeBoolean(ByteStream* pByteStrm, const char* elementTag, flag* value, int *pErrCode)
{
	char tmp[256];
	flag hasExtTag = (elementTag != NULL && (strlen(elementTag)>0));
	memset(tmp, 0x0, sizeof(tmp));

	if (hasExtTag)
		if (!Xer_DecodeComplexElementStart(pByteStrm, elementTag, NULL, pErrCode))
			return FALSE;

	if (!Xer_LA_NextElementTag(pByteStrm, tmp))
		return FALSE;

	if (strcmp(tmp, "true") == 0) {
		*value = TRUE;
		if (!Xer_DecodePrimitiveElement(pByteStrm, "true", tmp, pErrCode))
			return FALSE;
	}
	else {
		*value = FALSE;
		if (!Xer_DecodePrimitiveElement(pByteStrm, "false", tmp, pErrCode))
			return FALSE;
	}


	if (hasExtTag)
		if (!Xer_DecodeComplexElementEnd(pByteStrm, elementTag, pErrCode))
			return FALSE;
	return TRUE;
}

flag Xer_DecodeEnumerated(ByteStream* pByteStrm, const char* elementTag, char* value, int *pErrCode)
{
	char tmp[256];
	flag hasExtTag = (elementTag != NULL && (strlen(elementTag)>0));
	memset(tmp, 0x0, sizeof(tmp));

	if (hasExtTag)
		if (!Xer_DecodeComplexElementStart(pByteStrm, elementTag, NULL, pErrCode))
			return FALSE;

	if (!Xer_LA_NextElementTag(pByteStrm, value))
		return FALSE;

	if (!Xer_DecodePrimitiveElement(pByteStrm, value, tmp, pErrCode))
		return FALSE;

	if (hasExtTag)
		if (!Xer_DecodeComplexElementEnd(pByteStrm, elementTag, pErrCode))
			return FALSE;

	return TRUE;
}

flag Xer_DecodeReal(ByteStream* pByteStrm, const char* elementTag, double* value, int *pErrCode)
{
	char tmp[256];
	memset(tmp, 0x0, sizeof(tmp));
	if (!Xer_DecodePrimitiveElement(pByteStrm, elementTag, tmp, pErrCode))
		return FALSE;
	*value = atof(tmp);
	return TRUE;
}


flag Xer_DecodeString(ByteStream* pByteStrm, const char* elementTag, char* value, int *pErrCode)
{
	return Xer_DecodePrimitiveElement(pByteStrm, elementTag, value, pErrCode);
}


flag CharToNibble(char c, byte* pNibble) {
	*pNibble = 0;
	if (c >= '0' && c <= '9') {
		*pNibble = (byte)(c - '0');
		return TRUE;
	}
	if (c >= 'A' && c <= 'F') {
		*pNibble = (byte)(c - 'A' + 10);
		return TRUE;
	}
	if (c >= 'a' && c <= 'f') {
		*pNibble = (byte)(c - 'a' + 10);
		return TRUE;
	}
	return FALSE;
}

flag Xer_DecodeOctetString(ByteStream* pByteStrm, const char* elementTag, byte value[], int bufferMaxSize, int* nCount, int *pErrCode)
{
	char tmp[1024];
	int len = 0;
	int i;
	int j = 0;
	memset(tmp, 0x0, sizeof(tmp));
	if (!Xer_DecodePrimitiveElement(pByteStrm, elementTag, tmp, pErrCode))
		return FALSE;

	len = (int)strlen(tmp);

	for (i = 0; i<len; i++) {
		if (isspace(tmp[i]))
			continue;
		tmp[j++] = tmp[i];
	}

	len = j;

	for (i = 0; i<len && i / 2 < bufferMaxSize; i++) {
		byte nibble;
		if (!CharToNibble(tmp[i], &nibble))
			return FALSE;
		if (i % 2 == 0) {
			value[i / 2] = (byte)(nibble << 4);
		}
		else {
			value[i / 2] |= nibble;
		}
	}
	if (nCount != NULL)
		*nCount = len / 2 + len % 2;

	return TRUE;
}




flag Xer_DecodeObjectIdentifier(ByteStream* pByteStrm, const char* elementTag, Asn1ObjectIdentifier *pVal, int *pErrCode)
{
	char tmp[1024];
	int len = 0;
	int i;
	int j = 0;
	char delim[] = ".";
	memset(tmp, 0x0, sizeof(tmp));
	if (!Xer_DecodePrimitiveElement(pByteStrm, elementTag, tmp, pErrCode))
		return FALSE;

	len = (int)strlen(tmp);

	for (i = 0; i<len; i++) {
		if (isspace(tmp[i]))
			continue;
		tmp[j++] = tmp[i];
	}

	len = j;

	char *ptr = strtok(tmp, delim);
	i = 0;
	while (ptr != NULL && i < OBJECT_IDENTIFIER_MAX_LENGTH)
	{
		pVal->values[i++] = atoll(tmp);

		ptr = strtok(NULL, delim);
	}

	return TRUE;
}




















flag Xer_DecodeBitString(ByteStream* pByteStrm, const char* elementTag, byte value[], int bufferMaxSize, int* nCount, int *pErrCode)
{
	char tmp[2048];
	int len = 0;
	int i;
	int bytes;
	int j = 0;

	memset(tmp, 0x0, sizeof(tmp));
	if (!Xer_DecodePrimitiveElement(pByteStrm, elementTag, tmp, pErrCode))
		return FALSE;

	len = (int)strlen(tmp);
	for (i = 0; i<len; i++) {
		if (isspace(tmp[i]))
			continue;
		tmp[j++] = tmp[i];
	}
	len = j;

	bytes = len / 8;
	if (len % 8)
		bytes++;

	memset(value, 0x0, (size_t)bytes);


	for (i = 0; i<len && i / 8 < bufferMaxSize; i++) {
		byte curVal = (byte)(tmp[i] - '0');
		int curBit = 7 - i % 8;
		value[i / 8] |= (byte)(curVal << curBit);
	}
	if (nCount != NULL)
		*nCount = len;

	return TRUE;
}



flag Xer_NextEndElementIs(ByteStream* pByteStrm, const char* elementTag)
{
	Token t;
	int save = pByteStrm->currentByte;

	if (NT(pByteStrm).TokenID != '<') {
		pByteStrm->currentByte = save;
		return FALSE;
	}

	if (NT(pByteStrm).TokenID != '/') {
		pByteStrm->currentByte = save;
		return FALSE;
	}

	t = NT(pByteStrm);
	if ((t.TokenID != WORD_ID) || (strcmp(t.Value, elementTag) != 0)) {
		pByteStrm->currentByte = save;
		return FALSE;
	}

	if (NT(pByteStrm).TokenID != '>') {
		pByteStrm->currentByte = save;
		return FALSE;
	}


	pByteStrm->currentByte = save;
	return TRUE;
}


flag Xer_NextStartElementIs(ByteStream* pByteStrm, const char* elementTag)
{
	Token t;
	int save = pByteStrm->currentByte;

	if (NT(pByteStrm).TokenID != '<') {
		pByteStrm->currentByte = save;
		return FALSE;
	}

	t = NT(pByteStrm);
	if ((t.TokenID != WORD_ID) || (strcmp(t.Value, elementTag) != 0)) {
		pByteStrm->currentByte = save;
		return FALSE;
	}

	if (NT(pByteStrm).TokenID != '>') {
		pByteStrm->currentByte = save;
		return FALSE;
	}


	pByteStrm->currentByte = save;
	return TRUE;
}


flag Xer_LA_NextElementTag(ByteStream* pByteStrm, char* elementTag)
{
	Token t;
	int save = pByteStrm->currentByte;

	if (NT(pByteStrm).TokenID != '<') {
		pByteStrm->currentByte = save;
		return FALSE;
	}

	t = NT(pByteStrm);
	strcpy(elementTag, t.Value);
	if ((t.TokenID != WORD_ID) || (strcmp(t.Value, elementTag) != 0)) {
		pByteStrm->currentByte = save;
		return FALSE;
	}

	t = NT(pByteStrm);
	if (t.TokenID == '/') {
		if (NT(pByteStrm).TokenID == '>') {
			pByteStrm->currentByte = save;
			return TRUE;
		}
		else {
			pByteStrm->currentByte = save;
			return FALSE;
		}
	}
	else {
		if (t.TokenID != '>') {
			pByteStrm->currentByte = save;
			return FALSE;
		}
	}



	pByteStrm->currentByte = save;
	return TRUE;
}



flag Xer_EncodeComment(ByteStream* pByteStrm, const char* comment, int *pErrCode)
{
	*pErrCode = ERR_INVALID_XML_FILE;
	if (!ByteStream_AppendString(pByteStrm, "<!--"))
		return FALSE;

	if (!ByteStream_AppendString(pByteStrm, comment))
		return FALSE;

	if (!ByteStream_AppendString(pByteStrm, "-->"))
		return FALSE;
	return TRUE;
}

void Xer_EncodeXmlHeader(ByteStream* pByteStrm, const char* xmlHeader)
{
	const char* hdr = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
	if (xmlHeader != NULL)
		hdr = xmlHeader;

	strcpy((char*)pByteStrm->buf, hdr);
	pByteStrm->currentByte = (long)strlen(hdr);
	pByteStrm->buf[pByteStrm->currentByte++] = '\n';
}


/*
The following functions are used to load an XML file (by removing white spaces) into an ByteStream
*/



typedef enum {
	XmlStart = 0,
	XmlHeader = 1,
	XmlStartTag = 2,
	XmlContent = 3,
	XmlEndTag = 4,
	XmlMixedContent = 5,
	XmlComment = 6
} XmlState;

XmlState PreviousState = XmlStart;

flag ByteStream_AppendChar(ByteStream* pStrm, const char v)
{
	if (pStrm->currentByte >= pStrm->count)
		return FALSE;

	pStrm->buf[pStrm->currentByte] = (byte)v;
	pStrm->currentByte++;
	return TRUE;
}

typedef flag(*OnXmlStateFunc)(int c, int l1, ByteStream* pStrm, ByteStream* pTmpStrm, FILE* xmlFile, XmlState* pNewState);


flag OnXmlStart(int c, int l1, ByteStream* pStrm, ByteStream* pTmpStrm, FILE* xmlFile, XmlState* pNewState)
{
	(void)xmlFile;
	(void)pTmpStrm;

	if (c != '<') {
		*pNewState = XmlStart;
		return TRUE;
	}

	if (l1 == '!') {
		*pNewState = XmlComment;
		PreviousState = XmlStart;
		return TRUE;
	}

	if (l1 == '?') {
		*pNewState = XmlHeader;
		return TRUE;
	}

	if (!ByteStream_AppendChar(pStrm, (char)c))
		return FALSE;

	*pNewState = XmlStartTag;
	return TRUE;
}

flag OnXmlHeader(int c, int l1, ByteStream* pStrm, ByteStream* pTmpStrm, FILE* xmlFile, XmlState* pNewState)
{
	(void)xmlFile;
	(void)pTmpStrm;
	(void)l1;
	(void)pStrm;


	if (c == '?' && l1 == '>') {
		fgetc(xmlFile); //discard l1 --> '>';
		*pNewState = XmlStart;
	}
	else {
		*pNewState = XmlHeader;
	}

	return TRUE;
}


flag OnXmlStartTag(int c, int l1, ByteStream* pStrm, ByteStream* pTmpStrm, FILE* xmlFile, XmlState* pNewState)
{
	(void)xmlFile;
	(void)pTmpStrm;
	(void)l1;

	if (c == '>')
		*pNewState = XmlContent;
	else
		*pNewState = XmlStartTag;

	if (!ByteStream_AppendChar(pStrm, (char)c))
		return FALSE;

	return TRUE;
}

flag OnXmlContent(int c, int l1, ByteStream* pStrm, ByteStream* pTmpStrm, FILE* xmlFile, XmlState* pNewState)
{
	(void)xmlFile;

	if (c != '<')
	{
		*pNewState = XmlContent;
		if (!ByteStream_AppendChar(pTmpStrm, (char)c))
			return FALSE;
	}
	else {
		if (l1 == '!') {
			*pNewState = XmlComment;
			PreviousState = XmlContent;
			return TRUE;
		}

		if (l1 == '/') {
			int i = 0;
			*pNewState = XmlEndTag;
			//copy data from tmp buf to main steam
			for (i = 0; i<pTmpStrm->currentByte; i++)
				if (!ByteStream_AppendChar(pStrm, (char)pTmpStrm->buf[i]))
					return FALSE;
			//Discard tmp buf
			pTmpStrm->currentByte = 0;
		}
		else {
			*pNewState = XmlStartTag;
			//Discard tmp buf
			pTmpStrm->currentByte = 0;
		}
		if (!ByteStream_AppendChar(pStrm, (char)c))
			return FALSE;
	}

	return TRUE;
}


flag OnXmlEndTag(int c, int l1, ByteStream* pStrm, ByteStream* pTmpStrm, FILE* xmlFile, XmlState* pNewState)
{
	(void)xmlFile;
	(void)pTmpStrm;
	(void)l1;

	if (c == '>')
		*pNewState = XmlMixedContent;
	else
		*pNewState = XmlEndTag;

	if (!ByteStream_AppendChar(pStrm, (char)c))
		return FALSE;

	return TRUE;
}

flag OnXmlMixedContent(int c, int l1, ByteStream* pStrm, ByteStream* pTmpStrm, FILE* xmlFile, XmlState* pNewState)
{
	(void)xmlFile;
	(void)pTmpStrm;

	if (c != '<')
	{
		*pNewState = XmlMixedContent;
	}
	else {
		if (l1 == '!') {
			*pNewState = XmlComment;
			PreviousState = XmlMixedContent;
			return TRUE;
		}

		if (l1 == '/')
			*pNewState = XmlEndTag;
		else
			*pNewState = XmlStartTag;

		if (!ByteStream_AppendChar(pStrm, (char)c))
			return FALSE;
	}

	return TRUE;
}


flag OnXmlComment(int c, int l1, ByteStream* pStrm, ByteStream* pTmpStrm, FILE* xmlFile, XmlState* pNewState)
{
	(void)pTmpStrm;
	(void)pStrm;

	if (c == '-' && l1 == '>') {
		fgetc(xmlFile); //discard l1 i.e. '>';
		*pNewState = PreviousState;
	}
	else {
		*pNewState = XmlComment;
	}


	return TRUE;
}


#define TMP_BUFFER_SIZE 4096
flag LoadXmlFile(const char* fileName, ByteStream* pStrm, int* nBytesLoaded)
{
	int c;
	int l1;
	ByteStream tmpStrm;
	static byte tmpBuffer[TMP_BUFFER_SIZE];
	OnXmlStateFunc StateMachine[] = { OnXmlStart, OnXmlHeader, OnXmlStartTag, OnXmlContent, OnXmlEndTag, OnXmlMixedContent, OnXmlComment };

	XmlState curState = XmlStart;

	FILE* fp = fopen(fileName, "r");
	ByteStream_Init(&tmpStrm, tmpBuffer, TMP_BUFFER_SIZE);

	if (fp == NULL)
		return FALSE;
	while ((c = fgetc(fp)) != EOF) {
		l1 = fgetc(fp);
		ungetc(l1, fp);
		if (!(StateMachine[curState])(c, l1, pStrm, &tmpStrm, fp, &curState)) {
			fclose(fp);
			return FALSE;
		}
	}

	*nBytesLoaded = pStrm->currentByte;
	pStrm->currentByte = 0;
	fclose(fp);
	return TRUE;

}
