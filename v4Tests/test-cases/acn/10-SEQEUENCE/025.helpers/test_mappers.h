/* the mapping functions of the test (module test-mappers in the ACN) */
#ifndef TEST_MAPPERS_H
#define TEST_MAPPERS_H
#include "asn1crt.h"
asn1SccUint plus_encode(asn1SccUint value);
asn1SccUint plus_decode(asn1SccUint value);
asn1SccSint negate_encode(asn1SccSint value);
asn1SccSint negate_decode(asn1SccSint value);
asn1SccUint context_length_encode(asn1SccUint value);
asn1SccUint context_length_decode(asn1SccUint value);
#endif
