/* Byte-exact check of 010 (run by v4Tests/scripts/runWireTests.sh): the
   mapped length of a BIT STRING CONTAINING region is the region size in bits
   plus 2. */
#include <assert.h>

#include "010.h"

int main(void)
{
    byte encoded[Document_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    Document input;
    Document output;
    int error = 0;

    Document_Initialize(&input);
    input.payload.kind = other_PRESENT;
    input.payload.u.other.number = 42;

    BitStream_Init(&stream, encoded, sizeof encoded);
    assert(Document_ACN_Encode(&input, &stream, &error, TRUE));
    assert(encoded[0] == 10);   /* 8 bits + 2 */
    assert(encoded[1] == 2);    /* kind: other */
    assert(encoded[2] == 42);

    Document_Initialize(&output);
    BitStream_AttachBuffer(&stream, encoded, sizeof encoded);
    assert(Document_ACN_Decode(&output, &stream, &error));
    assert(Document_Equal(&input, &output));

    return 0;
}
