/* Byte-exact check of 017 (run by v4Tests/scripts/runWireTests.sh): len
   sizes head and the payload's data; the decoder reads head with it, and
   the encoder rejects a head whose size disagrees with the data's. */
#include <assert.h>
#include <string.h>

#include "017.h"

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
    static const byte expected[] = {2, 0xAA, 0xAA, 0xBB, 0xBB};
    byte encoded[Msg_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    Msg input;
    Msg output;
    long length;
    int error = 0;

    Msg_Initialize(&input);
    input.head.nCount = 2;
    memset(input.head.arr, 0xAA, 2);
    input.payload.data.nCount = 2;
    memset(input.payload.data.arr, 0xBB, 2);
    assert(encode(&input, encoded, sizeof encoded, &length) == 0);
    assert(length == sizeof expected && memcmp(encoded, expected, sizeof expected) == 0);
    Msg_Initialize(&output);
    BitStream_AttachBuffer(&stream, encoded, length);
    assert(Msg_ACN_Decode(&output, &stream, &error));
    assert(Msg_Equal(&input, &output));

    /* head of 3 bytes and data of 2: no single len. */
    input.head.nCount = 3;
    assert(encode(&input, encoded, sizeof encoded, &length) == ERR_ACN_DET_CONSISTENCY_MISMATCH);

    return 0;
}
