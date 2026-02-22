#ifndef ASN1SCC_ASN1CRT_H_
#define ASN1SCC_ASN1CRT_H_

#include <stddef.h>


#ifdef  __cplusplus
extern "C" {
#  include <cstdint>
#  include <cinttypes>
	/* C99 check */
#elif (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L) || _MSC_VER >= 1900
#  include <stdbool.h>
#  include <stdint.h>
#  include <inttypes.h>
#else /* No C++ nor C99 */
#  ifndef _MSC_VER
typedef unsigned char bool;
#    define true 1u
#    define false 0u
#  endif /* _MSC_VER */

typedef unsigned char uint8_t;

typedef int int32_t;
typedef unsigned int uint32_t;

typedef long long int64_t;
typedef unsigned long long uint64_t;

#endif	/* C++/C99 */

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

#ifndef NO_OF_BITS_IN_BYTE
#define NO_OF_BITS_IN_BYTE 8
#endif

#ifndef NO_OF_BYTES_IN_INT16
#define NO_OF_BYTES_IN_INT16 (sizeof(int16_t))	// 2
#endif

#ifndef NO_OF_BYTES_IN_INT32
#define NO_OF_BYTES_IN_INT32 (sizeof(int32_t))	// 4
#endif

#ifndef NO_OF_BITS_IN_INT16 
#define NO_OF_BITS_IN_INT16 (NO_OF_BYTES_IN_INT16 * NO_OF_BITS_IN_BYTE)	// 16
#endif

#ifndef NO_OF_BITS_IN_INT32 
#define NO_OF_BITS_IN_INT32 (NO_OF_BYTES_IN_INT32 * NO_OF_BITS_IN_BYTE)	// 32
#endif

#ifndef PRId32
#define PRId32 "d"
#endif

#ifndef PRId64
#define PRId64 "lld"
#endif

#ifndef PRIu32
#define PRIu32 "u"
#endif

#ifndef PRIu64
#define PRIu64 "llu"
#endif

#define OBJECT_IDENTIFIER_MAX_LENGTH	20


typedef float asn1Real32;
typedef double asn1Real64;

typedef uint8_t byte;

typedef int32_t asn1SccSint32;
typedef uint32_t asn1SccUint32;

typedef int64_t asn1SccSint64;
typedef uint64_t asn1SccUint64;

#if WORD_SIZE==8
typedef asn1SccUint64 asn1SccUint;
typedef asn1SccSint64 asn1SccSint;
#define ASN1SCC_PRId PRId64
#define ASN1SCC_PRIu PRIu64
#else
typedef asn1SccUint32 asn1SccUint;
typedef asn1SccSint32 asn1SccSint;
#define ASN1SCC_PRId PRId32
#define ASN1SCC_PRIu PRIu32
#endif

asn1SccUint int2uint(asn1SccSint v);
asn1SccSint uint2int(asn1SccUint v, int uintSizeInBytes);

#if FP_WORD_SIZE==8
typedef asn1Real64 asn1Real;
#else
typedef asn1Real32 asn1Real;
#endif

#if WORD_SIZE==8
#define MAX_INT 0xFFFFFFFFFFFFFFFFULL
#else
#define MAX_INT 0xFFFFFFFF
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

struct BitStream_t;

//typedef void(*PushDataFnc)(struct BitStream_t* pThis, void* pushDataPrm);
//typedef void(*FetchDataFnc)(struct BitStream_t* pThis, void* fetchDataPrm);

typedef struct BitStream_t {
	byte* buf;
	long count;
	long currentByte;
	/* Next available bit for writting.
	Possible vallues 0..7, 0 is most significant
	bit of current byte*/
	int currentBit;
	//PushDataFnc pushData;
	void* pushDataPrm;
	//FetchDataFnc fetchData;
	void* fetchDataPrm;
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



/**
 * Performs a binary search on a sorted array to find the index of a given value.
 *
 * @param arr The sorted array to search.
 * @param size The size of the array.
 * @param value The value to search for.
 * @return The index of the value if found, -1 otherwise.
 */
int binarySearch(const asn1SccSint arr[], int size, asn1SccSint value);

/**
 * Performs a linear search on an array to find a specific value.
 *
 * @param arr The array to be searched.
 * @param size The size of the array.
 * @param value The value to be searched for.
 * @return The index of the value if found, or -1 if not found.
 */
int linearSearch(const asn1SccSint arr[], int size, asn1SccSint value);


/* Time Classes
	Asn1LocalTime,					// TIME-OF-DAY  ::= TIME(SETTINGS "Basic=Time Time=HMS Local-or-UTC=L")
	Asn1UtcTime,					//                  TIME(SETTINGS "Basic=Time Time=HMS Local-or-UTC=Z")
	Asn1LocalTimeWithTimeZone,		//                  TIME(SETTINGS "Basic=Time Time=HMS Local-or-UTC=LD")
	Asn1Date,						//  DATE ::=        TIME(SETTINGS "Basic=Date Date=YMD Year=Basic")
	Asn1Date_LocalTime,				//  DATE-TIME   ::= TIME(SETTINGS "Basic=Date-Time Date=YMD Year=Basic Time=HMS Local-or-UTC=L")
	Asn1Date_UtcTime,				// 			        TIME(SETTINGS "Basic=Date-Time Date=YMD Year=Basic Time=HMS Local-or-UTC=Z")
	Asn1Date_LocalTimeWithTimeZone	//                  TIME(SETTINGS "Basic=Date-Time Date=YMD Year=Basic Time=HMS Local-or-UTC=LD")
*/
typedef struct {
	int sign;		//-1 or +1
	int hours;
	int mins;
} Asn1TimeZone;

typedef struct {
	int hours;
	int mins;
	int secs;
	int fraction;
	Asn1TimeZone tz;
} Asn1TimeWithTimeZone;

typedef struct {
	int hours;
	int mins;
	int secs;
	int fraction;
} Asn1UtcTime;

typedef struct {
	int hours;
	int mins;
	int secs;
	int fraction;
} Asn1LocalTime;

typedef struct {
	int years;
	int months;
	int days;
} Asn1Date;

typedef struct {
	Asn1Date	   date;
	Asn1LocalTime  time;
} Asn1DateLocalTime;

typedef struct {
	Asn1Date	 date;
	Asn1UtcTime  time;
} Asn1DateUtcTime;

typedef struct {
	Asn1Date	 date;
	Asn1TimeWithTimeZone  time;
} Asn1DateTimeWithTimeZone;

typedef enum {
	Asn1TC_LocalTimeStamp,
	Asn1TC_UtcTimeStamp,
	Asn1TC_LocalTimeTZStamp
} Asn1TimeZoneClass;

typedef asn1SccUint BerTag;

#if WORD_SIZE==8
extern const asn1SccUint64 ber_aux[];
#else
extern const asn1SccUint32 ber_aux[];
#endif

#define CHECK_BIT_STREAM(pBitStrm)		assert((pBitStrm)->currentByte*8+(pBitStrm)->currentBit<=(pBitStrm)->count*8)

// check if nBits still have space in pBitStrm (only for non streaming mode)
#ifndef ASN1SCC_STREAMING
#define CHECK_BIT_STREAM_PRE(pBitStrm, nBits)	assert( (long)((pBitStrm)->currentByte * 8 + (pBitStrm)->currentBit + (nBits)) <= ((pBitStrm)->count * 8))
#else
#define CHECK_BIT_STREAM_PRE(pBitStrm, nBits) ((void)pBitStrm)
#endif

#ifdef _MSC_VER
#pragma warning( disable : 4127)
#endif

#define ASSERT_OR_RETURN_FALSE(_Expression) do { assert(_Expression); if (!(_Expression)) return FALSE;} while(0)

#ifdef  __cplusplus
}
#endif


#endif
