/* the mapping functions take and return the C type of the field: the generated typedefs (sample1.h) follow the
   options (-slim: uint8_t / int8_t) */
#include "sample1.h"

/* plus: the wire carries the value + 2 */
ASN1SCC_Pct plus_encode(ASN1SCC_Pct value) { return (ASN1SCC_Pct)(value + 2); }
ASN1SCC_Pct plus_decode(ASN1SCC_Pct value) { return (ASN1SCC_Pct)(value - 2); }

/* negate: the wire carries -value */
ASN1SCC_Temperature negate_encode(ASN1SCC_Temperature value) { return (ASN1SCC_Temperature)(-value); }
ASN1SCC_Temperature negate_decode(ASN1SCC_Temperature value) { return (ASN1SCC_Temperature)(-value); }

/* context-length: the length is not on the wire (size 0); the decoder takes the one the
   encoder recorded, as a PUS parameter length known from the context */
static ASN1SCC_Length context_length = 0;
ASN1SCC_Length context_length_encode(ASN1SCC_Length value) { context_length = value; return 0; }
ASN1SCC_Length context_length_decode(ASN1SCC_Length value) { (void)value; return context_length; }
