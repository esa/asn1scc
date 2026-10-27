#include "test_mappers.h"

/* plus: the wire carries the value + 2 */
asn1SccUint plus_encode(asn1SccUint value) { return value + 2; }
asn1SccUint plus_decode(asn1SccUint value) { return value - 2; }

/* negate: the wire carries -value */
asn1SccSint negate_encode(asn1SccSint value) { return -value; }
asn1SccSint negate_decode(asn1SccSint value) { return -value; }

/* context-length: the length is not on the wire (size 0); the decoder takes the one the
   encoder recorded, as a PUS parameter length known from the context */
static asn1SccUint context_length = 0;
asn1SccUint context_length_encode(asn1SccUint value) { context_length = value; return 0; }
asn1SccUint context_length_decode(asn1SccUint value) { (void)value; return context_length; }
