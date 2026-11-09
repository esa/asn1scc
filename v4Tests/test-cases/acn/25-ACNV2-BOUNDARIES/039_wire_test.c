/* Byte-exact check of 039 (run by v4Tests/scripts/runWireTests.sh with --acn-v2, Issue 80): two CONTAINING components
   with external size fields in one SEQUENCE keep their own contents (legacy wrote one with the bytes of the other). */
#include <assert.h>
#include <string.h>

#include "039.h"

int main(void)
{
    /* l1 3, l2 5, p1 (a = 1, data 11), p2 (a = 2, data 21 22 23), each padded to whole octets */
    static const byte expectedTwo[] = {0x03, 0x05, 0x01, 0x04, 0x40, 0x02, 0x88, 0x48, 0x88, 0xC0};
    /* l 3, p1 (a = 1, data 11), p2 (a = 2, data 21) */
    static const byte expectedOne[] = {0x03, 0x01, 0x04, 0x40, 0x02, 0x08, 0x40};
    byte encodedTwo[Two_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    byte encodedOne[One_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    Two inTwo;
    Two outTwo;
    One inOne;
    One outOne;
    long length;
    int error = 0;

    Two_Initialize(&inTwo);
    inTwo.p1.a = 1;
    inTwo.p1.data.nCount = 1;
    inTwo.p1.data.arr[0] = 0x11;
    inTwo.p2.a = 2;
    inTwo.p2.data.nCount = 3;
    inTwo.p2.data.arr[0] = 0x21;
    inTwo.p2.data.arr[1] = 0x22;
    inTwo.p2.data.arr[2] = 0x23;
    BitStream_Init(&stream, encodedTwo, sizeof encodedTwo);
    assert(Two_ACN_Encode(&inTwo, &stream, &error, TRUE));
    length = BitStream_GetLength(&stream);
    assert(length == sizeof expectedTwo && memcmp(encodedTwo, expectedTwo, sizeof expectedTwo) == 0);
    Two_Initialize(&outTwo);
    BitStream_AttachBuffer(&stream, encodedTwo, length);
    assert(Two_ACN_Decode(&outTwo, &stream, &error));
    assert(Two_Equal(&inTwo, &outTwo));

    One_Initialize(&inOne);
    inOne.p1.a = 1;
    inOne.p1.data.nCount = 1;
    inOne.p1.data.arr[0] = 0x11;
    inOne.p2.a = 2;
    inOne.p2.data.nCount = 1;
    inOne.p2.data.arr[0] = 0x21;
    BitStream_Init(&stream, encodedOne, sizeof encodedOne);
    assert(One_ACN_Encode(&inOne, &stream, &error, TRUE));
    length = BitStream_GetLength(&stream);
    assert(length == sizeof expectedOne && memcmp(encodedOne, expectedOne, sizeof expectedOne) == 0);
    One_Initialize(&outOne);
    BitStream_AttachBuffer(&stream, encodedOne, length);
    assert(One_ACN_Decode(&outOne, &stream, &error));
    assert(One_Equal(&inOne, &outOne));

    return 0;
}
