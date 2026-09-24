/* Byte-exact check of 004 (run by v4Tests/scripts/runWireTests.sh): the
   mapped length is patched with plus_encode(region size). A wrong but
   self-consistent encoding passes the -atc round trip. */
#include <assert.h>

#include "004.h"

int main(void)
{
    byte encoded[Document_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    Document input;
    Document output;
    int error = 0;

    Document_Initialize(&input);
    input.payload.kind = value_PRESENT;
    input.payload.u.value.number = 42;

    BitStream_Init(&stream, encoded, sizeof encoded);
    assert(Document_ACN_Encode(&input, &stream, &error, TRUE));
    assert(encoded[0] == 3);
    assert(encoded[1] == 1);
    assert(encoded[2] == 42);

    Document_Initialize(&output);
    BitStream_AttachBuffer(&stream, encoded, sizeof encoded);
    assert(Document_ACN_Decode(&output, &stream, &error));
    assert(Document_Equal(&input, &output));

    return 0;
}
