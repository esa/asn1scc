/* Byte-exact check of 038 (run by v4Tests/scripts/runWireTests.sh with --acn-v2, Issue 78): the size field of an
   OCTET STRING (CONTAINING X) SEQUENCE component sized by an ACN parameter, the SEQUENCE being an alternative of a
   parameterized CHOICE, holds the length of the region when the alternative is selected, 0 otherwise. */
#include <assert.h>
#include <string.h>

#include "038.h"

static void roundTrip(const Msg* in, const byte* expected, long expectedLength)
{
    byte encoded[Msg_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    Msg out;
    long length;
    int error = 0;

    BitStream_Init(&stream, encoded, sizeof encoded);
    assert(Msg_ACN_Encode(in, &stream, &error, TRUE));
    length = BitStream_GetLength(&stream);
    assert(length == expectedLength && memcmp(encoded, expected, (size_t)expectedLength) == 0);
    Msg_Initialize(&out);
    BitStream_AttachBuffer(&stream, encoded, length);
    assert(Msg_ACN_Decode(&out, &stream, &error));
    assert(Msg_Equal(in, &out));
}

int main(void)
{
    /* len 2, CHOICE index 0 (s), Inner (a = 0xAB, b = 2: 10 bits) padded to 2 octets */
    static const byte expectedS[] = {0x02, 0x55, 0xC0, 0x00};
    /* len 0 (fallback), CHOICE index 1 (n), n = 9 */
    static const byte expectedN[] = {0x00, 0xC8};
    Msg in;

    Msg_Initialize(&in);
    in.c.kind = s_PRESENT;
    in.c.u.s.p.a = 0xAB;
    in.c.u.s.p.b = 2;
    roundTrip(&in, expectedS, sizeof expectedS);

    Msg_Initialize(&in);
    in.c.kind = n_PRESENT;
    in.c.u.n = 9;
    roundTrip(&in, expectedN, sizeof expectedN);

    return 0;
}
