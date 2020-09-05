#include <stdio.h>


#include <string.h>
#include <math.h>
#include <limits.h>
#include <stdio.h>
#include "asn1crt.h"
#include "a.h"


/*
The following two functions, with these specific names, must be implemented in user code.
There functions will be called by the asn1crt_encoding.c whenever data are needed.
*/

//called during decoding
void fetchData(BitStream* pBitStrm, void* pParam) {
	FILE* fp = (FILE*)pParam;
	memset(pBitStrm->buf, 0x0, pBitStrm->count);
	int bytes_read = fread(pBitStrm->buf, 1, pBitStrm->count, fp);
	printf("fetchData called, bytes read = %d\n", bytes_read);
}



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
	static byte encBuff[A1_REQUIRED_BYTES_FOR_ENCODING];
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

flag encode_with_streaming_mode(const A1* pVal, int* pErrCode, const char* filename) {
	static byte small_buff[1024];
	FILE* fp = fopen(filename, "wb");
	BitStream bitStrm;
	flag ret = TRUE;

	/*pass the small buffer and the file handler. The file handler will be passed to
	  pushData() function whenever the buffer is full*/
	BitStream_Init2(&bitStrm, small_buff, sizeof(small_buff), fp, NULL);
	ret = A1_Encode(pVal, &bitStrm, pErrCode, TRUE);
	if (!ret) {
		printf("Encoding failed !!!\n");
		return FALSE;
	}
	//write last remaining bytes
	if (bitStrm.currentByte > 0) {
		fwrite(bitStrm.buf, 1, bitStrm.currentByte, fp);
		printf("Last bytes written = %ld\n", bitStrm.currentByte);
	}
	fclose(fp);
	return true;


}

/*
decode with streaming mode.
Please note that the only difference between this function and decode_no_streaming_mode()
is that this function declares a smaller buffer (1K in this example vs 4K) and
when passing the buffer in the stream via BitStream_AttachBuffer2 it passes two
more parameters
*/
//called during encoding
void pushData(BitStream* pBitStrm, void* pParam) {
	FILE* fp = (FILE*)pParam;
	fwrite(pBitStrm->buf, 1, pBitStrm->currentByte, fp);
	printf("pushData called, bytes written = %ld\n", pBitStrm->currentByte);
}


flag decode_with_streaming_mode(A1* pVal, int* pErrCode, const char* filename) {
	BitStream bitStrm;
	static byte small_buff[1024];
	FILE* fp = fopen(filename, "rb");
	rsize_t bytes_read;
	flag ret = FALSE;
	if ((bytes_read = fread(small_buff, 1, sizeof(small_buff), fp)) > 0) {
		printf("Initially bytes read = %I64d\n", bytes_read);

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

flag decode_no_streaming_mode(A1* pVal, int* pErrCode, const char* filename) {
	BitStream bitStrm;
	static byte big_buff[A1_REQUIRED_BYTES_FOR_ENCODING];
	FILE* fp = fopen(filename, "rb");
	rsize_t bytes_read;
	flag ret = FALSE;
	if ((bytes_read = fread(big_buff, 1, sizeof(big_buff), fp)) > 0) {
		BitStream_AttachBuffer(&bitStrm, big_buff, bytes_read);
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



int scenario_1(const A1* pEncodedPdu) {
	int errCode;
	static A1 decodedPDU;
	flag ret;
	// encode the PDU using non streaming mode
	ret = encode_no_streaming_mode(pEncodedPdu,&errCode, "foo2.bin");
	if (ret) {
		// decode the data into the decoded PDU using streaming mode
		ret = decode_with_streaming_mode(&decodedPDU, &errCode, "foo2.bin");
		if (ret) {
			// compare the two PDUs
			ret = A1_Equal(pEncodedPdu, &decodedPDU);
			if (ret) {
				printf("Encode and Decoded PDUs are equal\n");
			} else {
				printf("Oh no ... Encode and Decoded PDUs are NOT equal\n");
			}
		}

	}
	return ret;
}

int scenario_2(const A1* pEncodedPdu) {
	int errCode;
	static A1 decodedPDU;
	flag ret;
	// encode the PDU using streaming mode
	ret = encode_with_streaming_mode(pEncodedPdu,&errCode, "foo3.bin");
	if (ret) {
		// decode the data into the decoded PDU using non streaming mode
		ret = decode_no_streaming_mode(&decodedPDU, &errCode, "foo3.bin");
		if (ret) {
			// compare the two PDUs
			ret = A1_Equal(pEncodedPdu, &decodedPDU);
			if (ret) {
				printf("Encode and Decoded PDUs are equal\n");
			} else {
				printf("Oh no ... Encode and Decoded PDUs are NOT equal\n");
			}
		}

	}
	return ret;
}


int main(int argc, char* argv[])
{
    (void)argc;
    (void)argv;
	flag ret;
	static A1 encodedPud;

	//initialize the encoded PDU with some values
	initialize(&encodedPud);

	//scenario 1
	//encoded the PDU using non streaming mode
	//and then decode using the  streaming mode
	ret = scenario_1(&encodedPud);
	if (ret) {
		printf("Scenario 1 succeeded\n");
		//scenario 2
		//encoded the PDU using streaming mode
		//and then decode using non  streaming mode
		ret = scenario_2(&encodedPud);
		if (ret) {
			printf("Scenario 2 succeeded\n");
		}
	}



    
    return 0;
}
