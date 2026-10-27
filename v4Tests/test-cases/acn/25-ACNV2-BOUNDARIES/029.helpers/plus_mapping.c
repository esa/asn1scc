#include "asn1crt.h"

/* mapping-function plus: the wire carries the length + 2 */
asn1SccUint plus_encode(asn1SccUint value) { return value + 2; }
asn1SccUint plus_decode(asn1SccUint value) { return value - 2; }
