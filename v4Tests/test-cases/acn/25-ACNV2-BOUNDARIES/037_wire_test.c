/* Byte-exact check of 037 (run by v4Tests/scripts/runWireTests.sh, with and without --acn-v2, Issue 78): the size
   field of an OCTET STRING (CONTAINING X) SEQUENCE component sized by an ACN parameter of its SEQUENCE holds the
   length of the region. The round trip alone cannot see it: before the fix acn-v2 wrote 0 and its decoder ignored
   the field. */
#include <assert.h>
#include <string.h>

#include "037.h"

int main(void)
{
    /* flag 101, len 2, x 110, Inner (a = 0xAB, b = 2: 10 bits) padded to 2 octets, y 1 */
    static const byte expected1[] = {0xA0, 0x5A, 0xAE, 0x02};
    /* len 2, Inner padded to 2 octets */
    static const byte expected2[] = {0x02, 0xAB, 0x80};
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
    in1.w.x = 6;
    in1.w.p.a = 0xAB;
    in1.w.p.b = 2;
    in1.w.y = TRUE;
    BitStream_Init(&stream, encoded1, sizeof encoded1);
    assert(Msg1_ACN_Encode(&in1, &stream, &error, TRUE));
    length = BitStream_GetLength(&stream);
    assert(length == sizeof expected1 && memcmp(encoded1, expected1, sizeof expected1) == 0);
    Msg1_Initialize(&out1);
    BitStream_AttachBuffer(&stream, encoded1, length);
    assert(Msg1_ACN_Decode(&out1, &stream, &error));
    assert(Msg1_Equal(&in1, &out1));

    Msg2_Initialize(&in2);
    in2.w.m.p.a = 0xAB;
    in2.w.m.p.b = 2;
    BitStream_Init(&stream, encoded2, sizeof encoded2);
    assert(Msg2_ACN_Encode(&in2, &stream, &error, TRUE));
    length = BitStream_GetLength(&stream);
    assert(length == sizeof expected2 && memcmp(encoded2, expected2, sizeof expected2) == 0);
    Msg2_Initialize(&out2);
    BitStream_AttachBuffer(&stream, encoded2, length);
    assert(Msg2_ACN_Decode(&out2, &stream, &error));
    assert(Msg2_Equal(&in2, &out2));

    /* len 1, but Inner takes 10 bits: the region decoder must reject it (before the fix the acn-v2 decoder ignored
       the field and accepted the stream) */
    {
        static byte overrun[] = {0x01, 0xAB, 0x80};
        Msg2_Initialize(&out2);
        BitStream_AttachBuffer(&stream, overrun, sizeof overrun);
        assert(!Msg2_ACN_Decode(&out2, &stream, &error));
    }

    return 0;
}
