

#include <string.h>
#include <assert.h>
#include <math.h>
#include <float.h>
#include <stdio.h>

#include "asn1crt.h"

void ObjectIdentifier_subidentifiers_uper_encode(byte* encodingBuf, int* pSize, asn1SccUint siValue);

void testObjectIdentifier_subidentifiers_uper_encode() {
	byte buf[1024];
	int size;
	ObjectIdentifier_subidentifiers_uper_encode(buf, &size, 18446744073709551615UL);
}

int main(char* argv, int* argc) {


	BitStream bitStrm;
	BitStream bitStrm2;
	byte buf[1024];
	Asn1ObjectIdentifier oi;
	Asn1ObjectIdentifier oi2;

	testObjectIdentifier_subidentifiers_uper_encode();
	return 1;




	//byte buf2[1024];
	//int nSize = 0;
	//ObjectIdentifier_subidentifiers_uper_encode(buf2, &nSize, 256);


	BitStream_Init(&bitStrm, buf, 1024);

	ObjectIdentifier_Init(&oi);
	
	oi.nCount = 15;

	for (int i = 0; i < oi.nCount; i++) {
		oi.values[i] = i*i*i*i;
	}
	//oi.nCount = 1;
	//oi.values[0] = 256;

	ObjectIdentifier_uper_encode(&bitStrm, &oi);

	//decode
	BitStream_AttachBuffer(&bitStrm2, buf, 1024);

	if (!ObjectIdentifier_uper_decode(&bitStrm2, &oi2)) {
		printf("error while decoding\n");
	}

	if (!ObjectIdentifier_equal(&oi,&oi2)) {
		printf("decdoging value differs!!!\n");
	}

	


	printf("OK ...........!!!\n");


	return 0;
}


