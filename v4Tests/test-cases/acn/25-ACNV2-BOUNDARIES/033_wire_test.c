/* Byte-exact check of 033 (run by v4Tests/scripts/runWireTests.sh, with and without --acn-v2, Issue 65):
   an ACN inserted BOOLEAN writes its true-value / false-value pattern, also as a deferred determinant. */
#include <assert.h>
#include <string.h>

#include "033.h"

static void roundTrip1(const Msg1* in, const byte* expected, long expectedLength)
{
    byte encoded[Msg1_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    Msg1 out;
    long length;
    int error = 0;

    BitStream_Init(&stream, encoded, sizeof encoded);
    assert(Msg1_ACN_Encode(in, &stream, &error, TRUE));
    length = BitStream_GetLength(&stream);
    assert(length == expectedLength && memcmp(encoded, expected, (size_t)expectedLength) == 0);
    Msg1_Initialize(&out);
    BitStream_AttachBuffer(&stream, encoded, length);
    assert(Msg1_ACN_Decode(&out, &stream, &error));
    assert(Msg1_Equal(in, &out));
}

static void roundTrip2(const Msg2* in, const byte* expected, long expectedLength)
{
    byte encoded[Msg2_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    Msg2 out;
    long length;
    int error = 0;

    BitStream_Init(&stream, encoded, sizeof encoded);
    assert(Msg2_ACN_Encode(in, &stream, &error, TRUE));
    length = BitStream_GetLength(&stream);
    assert(length == expectedLength && memcmp(encoded, expected, (size_t)expectedLength) == 0);
    Msg2_Initialize(&out);
    BitStream_AttachBuffer(&stream, encoded, length);
    assert(Msg2_ACN_Decode(&out, &stream, &error));
    assert(Msg2_Equal(in, &out));
}

int main(void)
{
    /* pa true 101, pb false 0110, pc true 0, a = 5 (101), c = 3 (011) */
    static const byte expected1a[] = {0xAC, 0xAC};
    /* pa false 010, pb true 1001, pc false 1, b = 7 (111) */
    static const byte expected1b[] = {0x53, 0xE0};
    /* pr true 10, inr.opt = 6 (110), tail = 9 (1001) */
    static const byte expected2a[] = {0xB4, 0x80};
    /* pr false 01, tail = 9 (1001) */
    static const byte expected2b[] = {0x64};
    Msg1 in1;
    Msg2 in2;

    Msg1_Initialize(&in1);
    in1.exist.a = 1;
    in1.a = 5;
    in1.exist.b = 0;
    in1.exist.c = 1;
    in1.c = 3;
    roundTrip1(&in1, expected1a, sizeof expected1a);

    Msg1_Initialize(&in1);
    in1.exist.a = 0;
    in1.exist.b = 1;
    in1.b = 7;
    in1.exist.c = 0;
    roundTrip1(&in1, expected1b, sizeof expected1b);

    Msg2_Initialize(&in2);
    in2.inr.exist.opt = 1;
    in2.inr.opt = 6;
    in2.tail = 9;
    roundTrip2(&in2, expected2a, sizeof expected2a);

    Msg2_Initialize(&in2);
    in2.inr.exist.opt = 0;
    in2.tail = 9;
    roundTrip2(&in2, expected2b, sizeof expected2b);

    return 0;
}
