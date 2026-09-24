/* Byte-exact check of 011 (run by v4Tests/scripts/runWireTests.sh): the
   mapped length of a plain OCTET STRING CONTAINING region is its size in
   bytes plus 2, and the trailer follows the region. */
#include <assert.h>

#include "011.h"

int main(void)
{
    byte encoded[Document_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    Document input;
    Document output;
    int error = 0;

    Document_Initialize(&input);
    input.payload.number = 42;
    input.payload.extra = 0x1234;
    input.trailer = 7;

    BitStream_Init(&stream, encoded, sizeof encoded);
    assert(Document_ACN_Encode(&input, &stream, &error, TRUE));
    assert(encoded[0] == 5);    /* 3 bytes + 2 */
    assert(encoded[1] == 42);
    assert(encoded[2] == 0x12);
    assert(encoded[3] == 0x34);
    assert(encoded[4] == 7);

    Document_Initialize(&output);
    BitStream_AttachBuffer(&stream, encoded, sizeof encoded);
    assert(Document_ACN_Decode(&output, &stream, &error));
    assert(Document_Equal(&input, &output));

    return 0;
}
