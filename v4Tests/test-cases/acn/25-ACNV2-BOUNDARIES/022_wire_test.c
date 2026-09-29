#include <stdio.h>
#include <string.h>
#include "022.h"

static int test_octets(const unsigned char input[3], int valid) {
    unsigned char buffer[3];
    BitStream stream;
    OuterOct value;
    int error = 0;
    flag decoded;

    memcpy(buffer, input, sizeof buffer);
    memset(&value, 0, sizeof value);
    BitStream_AttachBuffer(&stream, buffer, sizeof buffer);
    decoded = OuterOct_ACN_Decode(&value, &stream, &error);
    if (valid) {
        if (!decoded || error != 0 || stream.currentByte != 3 ||
            value.body.data.nCount != 2 ||
            memcmp(value.body.data.arr, buffer + 1, 2) != 0)
            goto failed;
    } else if (decoded || error == 0 || stream.count != 3 || stream.currentByte != 1) {
        goto failed;
    }
    return 1;

failed:
    fprintf(stderr, "OCTET STRING valid=%d decoded=%d error=%d position=%ld limit=%ld\n",
            valid, decoded, error, stream.currentByte, stream.count);
    return 0;
}

static int test_bits(const unsigned char input[3], int valid) {
    unsigned char buffer[3];
    BitStream stream;
    OuterBits value;
    int error = 0;
    flag decoded;

    memcpy(buffer, input, sizeof buffer);
    memset(&value, 0, sizeof value);
    BitStream_AttachBuffer(&stream, buffer, sizeof buffer);
    decoded = OuterBits_ACN_Decode(&value, &stream, &error);
    if (valid) {
        if (!decoded || error != 0 || stream.currentByte != 3 ||
            value.body.data.nCount != 2 ||
            memcmp(value.body.data.arr, buffer + 1, 2) != 0)
            goto failed;
    } else if (decoded || error == 0 || stream.count != 3 || stream.currentByte != 1) {
        goto failed;
    }
    return 1;

failed:
    fprintf(stderr, "BIT STRING valid=%d decoded=%d error=%d position=%ld limit=%ld\n",
            valid, decoded, error, stream.currentByte, stream.count);
    return 0;
}

int main(void) {
    static const unsigned char good_octets[3] = {2, 0xAA, 0xBB};
    static const unsigned char bad_octets[3] = {10, 0xAA, 0xBB};
    static const unsigned char good_bits[3] = {16, 0xAA, 0xBB};
    static const unsigned char bad_bits[3] = {80, 0xAA, 0xBB};
    int passed = 1;

    passed &= test_octets(good_octets, 1);
    passed &= test_octets(bad_octets, 0);
    passed &= test_bits(good_bits, 1);
    passed &= test_bits(bad_bits, 0);
    return passed ? 0 : 1;
}
