/* Byte-exact check of 019 (run by v4Tests/scripts/runWireTests.sh):
   the fixed-size head and payload data have no nCount member. */
#include <assert.h>
#include <string.h>

#include "019.h"

int main(void)
{
    static const byte expected[] = {2, 0xAA, 0xAA, 0xBB, 0xBB};
    byte encoded[Msg_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    Msg input;
    Msg output;
    long length;
    int error = 0;

    Msg_Initialize(&input);
    memset(input.head.arr, 0xAA, sizeof input.head.arr);
    memset(input.payload.data.arr, 0xBB, sizeof input.payload.data.arr);
    BitStream_Init(&stream, encoded, sizeof encoded);
    assert(Msg_ACN_Encode(&input, &stream, &error, TRUE));
    length = BitStream_GetLength(&stream);
    assert(length == sizeof expected && memcmp(encoded, expected, sizeof expected) == 0);

    Msg_Initialize(&output);
    BitStream_AttachBuffer(&stream, encoded, length);
    assert(Msg_ACN_Decode(&output, &stream, &error));
    assert(Msg_Equal(&input, &output));

    return 0;
}
