#include <stdio.h>
#include <string.h>
#include "asn1crt_encoding.h"
#define CHECK(c) do { if (!(c)) { fprintf(stderr, "FAIL streaming line %d\n", __LINE__); return 1; } } while (0)

typedef struct { byte data[8]; int offset; } Transfer;
void fetchData(BitStream* bs, void* param) {
    Transfer* t = param;
    if (t->offset >= 8) { bs->count = 0; return; }
    memcpy(bs->buf, t->data + t->offset, 2);
    t->offset += 2;
    bs->count = 2;
}
void pushData(BitStream* bs, void* param) {
    Transfer* t = param;
    if (t->offset <= 6) {
        memcpy(t->data + t->offset, bs->buf, 2);
        t->offset += 2;
    }
}
int main(void) {
    const byte expected[8] = {1,2,3,4,5,6,7,8};
    byte chunk[2], decoded[9] = {0};
    Transfer transfer = {{0},0};
    BitStream bs;
    BitStream_Init2(&bs, chunk, 2, &transfer, NULL);
    CHECK(BitStream_EncodeOctetString_no_length(&bs, expected, 8));
    CHECK(transfer.offset == 8 && !memcmp(transfer.data, expected, 8));
    transfer.offset = 2;
    memcpy(chunk, expected, 2);
    BitStream_AttachBuffer2(&bs, chunk, 2, NULL, &transfer);
    CHECK(BitStream_DecodeOctetString_no_length(&bs, decoded, 8));
    CHECK(!memcmp(decoded, expected, 8));
    CHECK(!BitStream_DecodeOctetString_no_length(&bs, decoded, 1));
    transfer.offset = 2;
    memcpy(chunk, expected, 2);
    BitStream_AttachBuffer2(&bs, chunk, 2, NULL, &transfer);
    bs.currentBit = 3;
    CHECK(BitStream_ReadByteArray(&bs, decoded, 7));
    for (int i = 0; i < 7; i++) CHECK(decoded[i] == (byte)((expected[i] << 3) | (expected[i+1] >> 5)));
    CHECK(!BitStream_ReadByte(&bs, decoded));
    BitStream_AttachBuffer(&bs, chunk, 2);
    CHECK(!BitStream_DecodeOctetString_no_length(&bs, decoded, 3));
    puts("PASS streaming");
    return 0;
}
