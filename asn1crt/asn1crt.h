#ifndef ASN1SCC_ASN1CRT_H_
#define ASN1SCC_ASN1CRT_H_

#include <stddef.h>

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

#define OBJECT_IDENTIFIER_MAX_LENGTH	20

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

typedef struct {
	int nCount;
	asn1SccUint values[OBJECT_IDENTIFIER_MAX_LENGTH];
} Asn1ObjectIdentifier;

#define ERR_INSUFFICIENT_DATA	101
#define ERR_INCORRECT_PER_STREAM	102
#define ERR_INVALID_CHOICE_ALTERNATIVE	103
#define ERR_INVALID_ENUM_VALUE	104
#define ERR_INVALID_XML_FILE	200
#define ERR_INVALID_BER_FILE	201
#define ERR_BER_LENGTH_MISMATCH	202



flag OctetString_equal(int len1, int len2, const byte arr1[], const byte arr2[]);
flag BitString_equal(int nBitsLength1, int nBitsLength2, const byte arr1[], const byte arr2[]);
void ObjectIdentifier_Init(Asn1ObjectIdentifier *pVal);
flag ObjectIdentifier_equal(const Asn1ObjectIdentifier *pVal1, const Asn1ObjectIdentifier *pVal2);
flag ObjectIdentifier_isValid(const Asn1ObjectIdentifier *pVal);
flag RelativeOID_isValid(const Asn1ObjectIdentifier *pVal);


int GetCharIndex(char ch, byte allowedCharSet[], int setLen);







typedef asn1SccUint BerTag;






#if WORD_SIZE==8
extern const asn1SccUint64 ber_aux[];
#else
extern const asn1SccUint32 ber_aux[];
#endif




#define CHECK_BIT_STREAM(pBitStrm)	assert((pBitStrm)->currentByte*8+(pBitStrm)->currentBit<=(pBitStrm)->count*8)

#ifdef _MSC_VER
#pragma warning( disable : 4127)
#endif

#define ASSERT_OR_RETURN_FALSE(_Expression) do { assert(_Expression); if (!(_Expression)) return FALSE;} while(0)

#ifdef  __cplusplus
}
#endif


#endif
