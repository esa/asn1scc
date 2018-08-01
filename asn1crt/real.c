#include <assert.h>
#include <float.h>
#include <math.h>
#include "asn1crt.h"


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


void CalculateMantissaAndExponent(asn1Real d, int* exponent, asn1SccUint64* mantissa)
{

#if FP_WORD_SIZE==8
	union {
		asn1Real in;
		asn1SccUint64 out;
	} double2uint;
	asn1SccUint64 ll = 0;
#else
	union {
		asn1Real in;
		asn1SccUint32 out;
	} double2uint;
	asn1SccUint32 ll = 0;
#endif

	double2uint.in = d;
	ll = double2uint.out;

	*exponent = 0;
	*mantissa = 0;

#if FP_WORD_SIZE==8
	* exponent = (int)(((ll & ExpoBitMask) >> 52) - 1023 -52);
	*mantissa = ll & MantBitMask;
	(*mantissa) |= MantisaExtraBit;
#else
	*exponent = (int)(((ll & ExpoBitMask) >> 23) - 127 - 23);

	*mantissa = ll & MantBitMask;
	(*mantissa) |= MantisaExtraBit;
#endif
}

asn1Real GetDoubleByMantissaAndExp(asn1SccUint mantissa, int exponent)
{
	asn1Real ret = 1.0;
	if (mantissa == 0)
		return 0.0;

	if (exponent >= 0) {
		while (exponent) {
			ret = ret * 2.0;
			exponent--;
		}
		return (asn1Real)mantissa*ret;
	}
	else {
		exponent = -exponent;
		while (exponent) {
			ret = ret * 2.0;
			exponent--;
		}
		return ((asn1Real)mantissa) / ret;
	}
}

