/* Byte-exact check of 030 (run by v4Tests/scripts/runWireTests.sh, with and without --acn-v2, Issue 23):
   an OCTET STRING (CONTAINING X) region is whole octets from its first bit, also when that bit is not the
   first of a byte, and the contained encoding is padded with zeros up to them, as legacy writes it. */
#include <assert.h>
#include <string.h>

#include "030.h"

int main(void)
{
    /* flag 101, then Inner (a = 0xAB, b = 2: 10 bits) padded to 2 octets */
    static const byte expected1[] = {0xB5, 0x70, 0x00};
    /* flag 101, len 2, Inner padded to 2 octets, tail 9 */
    static const byte expected2[] = {0xA0, 0x55, 0x70, 0x12};
    byte encoded1[Msg1_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    byte encoded2[Msg2_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    Msg1 in1;
    Msg1 out1;
    Msg2 in2;
    Msg2 out2;
    long length;
    int error = 0;

    Msg1_Initialize(&in1);
    in1.flag = 5;
    in1.payload.a = 0xAB;
    in1.payload.b = 2;
    BitStream_Init(&stream, encoded1, sizeof encoded1);
    assert(Msg1_ACN_Encode(&in1, &stream, &error, TRUE));
    length = BitStream_GetLength(&stream);
    assert(length == sizeof expected1 && memcmp(encoded1, expected1, sizeof expected1) == 0);
    Msg1_Initialize(&out1);
    BitStream_AttachBuffer(&stream, encoded1, length);
    assert(Msg1_ACN_Decode(&out1, &stream, &error));
    assert(Msg1_Equal(&in1, &out1));

    Msg2_Initialize(&in2);
    in2.flag = 5;
    in2.payload.a = 0xAB;
    in2.payload.b = 2;
    in2.tail = 9;
    BitStream_Init(&stream, encoded2, sizeof encoded2);
    assert(Msg2_ACN_Encode(&in2, &stream, &error, TRUE));
    length = BitStream_GetLength(&stream);
    assert(length == sizeof expected2 && memcmp(encoded2, expected2, sizeof expected2) == 0);
    Msg2_Initialize(&out2);
    BitStream_AttachBuffer(&stream, encoded2, length);
    assert(Msg2_ACN_Decode(&out2, &stream, &error));
    assert(Msg2_Equal(&in2, &out2));

    /* len 1, but Inner takes 10 bits: its decoder reads 2 bits past the 1-octet region (into the rest of the
       region's last byte). The decoder must reject it; the --acn-v2 C decoder accepted it (a = 0xAB, b = 2, tail
       decoded from bit 19). */
    {
        static byte overrun[] = {0xA0, 0x35, 0x72, 0x00};
        Msg2_Initialize(&out2);
        BitStream_AttachBuffer(&stream, overrun, sizeof overrun);
        assert(!Msg2_ACN_Decode(&out2, &stream, &error));
    }

    return 0;
}
