/* Byte-exact check of 016 (run by v4Tests/scripts/runWireTests.sh): len,
   the size of every element's data, is written before the elements and
   patched by them; with no element it is 0; elements of different sizes
   cannot be encoded. */
#include <assert.h>
#include <string.h>

#include "016.h"

static int encode(const Msg *input, byte *encoded, size_t size, long *length)
{
    BitStream stream;
    int error = 0;
    flag ok;
    BitStream_Init(&stream, encoded, (long)size);
    ok = Msg_ACN_Encode(input, &stream, &error, TRUE);
    *length = BitStream_GetLength(&stream);
    return ok ? 0 : error;
}

int main(void)
{
    static const byte expected[] = {2, 3, 0xA0, 0xA0, 0xA1, 0xA1, 0xA2, 0xA2};
    byte encoded[Msg_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    Msg input;
    Msg output;
    long length;
    int error = 0, i;

    Msg_Initialize(&input);
    input.items.nCount = 3;
    for (i = 0; i < 3; i++) {
        input.items.arr[i].data.nCount = 2;
        memset(input.items.arr[i].data.arr, 0xA0 + i, 2);
    }
    assert(encode(&input, encoded, sizeof encoded, &length) == 0);
    assert(length == sizeof expected && memcmp(encoded, expected, sizeof expected) == 0);
    Msg_Initialize(&output);
    BitStream_AttachBuffer(&stream, encoded, length);
    assert(Msg_ACN_Decode(&output, &stream, &error));
    assert(Msg_Equal(&input, &output));

    /* No element: len has no value to take and is written as 0. */
    input.items.nCount = 0;
    assert(encode(&input, encoded, sizeof encoded, &length) == 0);
    assert(length == 2 && encoded[0] == 0 && encoded[1] == 0);

    /* One len cannot size data of 2 and 3 bytes. */
    input.items.nCount = 2;
    input.items.arr[1].data.nCount = 3;
    assert(encode(&input, encoded, sizeof encoded, &length) == ERR_ACN_DET_CONSISTENCY_MISMATCH);

    return 0;
}
