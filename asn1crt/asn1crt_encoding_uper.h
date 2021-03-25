#ifndef ASN1SCC_ASN1CRT_ENCODING_UPER_H_
#define ASN1SCC_ASN1CRT_ENCODING_UPER_H_

#include "asn1crt_encoding.h"

#ifdef  __cplusplus
extern "C" {
#endif


void ObjectIdentifier_uper_encode(BitStream* pBitStrm, const Asn1ObjectIdentifier *pVal);
flag ObjectIdentifier_uper_decode(BitStream* pBitStrm, Asn1ObjectIdentifier *pVal);
void RelativeOID_uper_encode(BitStream* pBitStrm, const Asn1ObjectIdentifier *pVal);
flag RelativeOID_uper_decode(BitStream* pBitStrm, Asn1ObjectIdentifier *pVal);



#ifdef  __cplusplus
}
#endif


#endif
