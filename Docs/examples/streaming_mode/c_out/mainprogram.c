#include <stdio.h>


#include <string.h>
#include <math.h>
#include <limits.h>
#include <stdio.h>
#include "asn1crt.h"
#include "a.h"


void initialize(A1* pPdu) {
	int i1 = 0;
	while (i1< 1000) {
        /*set a11 */
        pPdu->arr[i1].a11 = i1*10;
        /*set a12 */
        pPdu->arr[i1].a12 = i1*50;
		i1 = i1 + 1;
	}
}

flag encode_no_streaming_mode(const A1* pVal, int* pErrCode, const char* filename) {
	static byte encBuff[A1_REQUIRED_BYTES_FOR_ENCODING]; /* +1 for zerosized types */
	BitStream bitStrm;
	flag ret = TRUE;

	BitStream_Init(&bitStrm, encBuff, A1_REQUIRED_BYTES_FOR_ENCODING);
	// Encode value
	ret = A1_Encode(pVal, &bitStrm, pErrCode, TRUE);
	if (!ret) {
		printf("Encoding failed !!!\n");
		return FALSE;
	}
	FILE* fp = fopen(filename, "wb");
	fwrite(bitStrm.buf, 1, bitStrm.count, fp);
	fclose(fp);
	return true;
}

/*
The following two functions, with these specific names, must be implemented in user code.
There functions will be called by the asn1crt_encoding.c whenever data are needed.
*/
void fetchData(BitStream* pBitStrm, void* pParam) {
	FILE* fp = (FILE*)pParam;
	memset(pBitStrm->buf, 0x0, pBitStrm->count);
	int bytes_read = fread(pBitStrm->buf, 1, pBitStrm->count, fp);
	printf("fetchDataFromFile called. bytes_read = %d\n", bytes_read);
}

void pushData(BitStream* pBitStrm, void* fp) {
	(void)pBitStrm;
	(void)fp;
}

flag decode_with_streaming_mode(A1* pVal, int* pErrCode, const char* filename) {
	BitStream bitStrm;
	static byte small_buff[1024];
	FILE* fp = fopen(filename, "rb");
	rsize_t bytes_read;
	flag ret = FALSE;
	if ((bytes_read = fread(small_buff, 1, sizeof(small_buff), fp)) > 0) {
		BitStream_AttachBuffer2(&bitStrm, small_buff, bytes_read, NULL, fp);
		ret = A1_Decode(pVal, &bitStrm, pErrCode);
		if (ret) {
			printf("Decoding succeded !!!\n");
		}
		else {
			printf("Decoding failed !!!\n");
		}
	}
	fclose(fp);
	return ret;
}


int main(int argc, char* argv[])
{
    (void)argc;
    (void)argv;
	flag ret;
	int errCode;
	static A1 encodedPDF;
	static A1 decodedPDU;

	//1. initialize the encoded PDU
	initialize(&encodedPDF);

	//2. encode the PDU using non streaming mode
	ret = encode_no_streaming_mode(&encodedPDF,&errCode, "foo2.bin");
	if (ret) {
		//3. decode the data into the decoded PDU using streaming mode
		ret = decode_with_streaming_mode(&decodedPDU, &errCode, "foo2.bin");
		if (ret) {
			//4. compare the two PDUs
			ret = A1_Equal(&encodedPDF, &decodedPDU);
			if (ret) {
				printf("Encode and Decoded PDUs are equal\n");
			} else {
				printf("Oh no ... Encode and Decoded PDUs are NOT equal\n");
			}
		}

	}

    //printf("running the automatic test cases to generated the the file 'test_case_UPER_000001.dat' ~ 18KB");
    //asn1scc_run_generated_testsuite(&output);


    
    return 0;
}
