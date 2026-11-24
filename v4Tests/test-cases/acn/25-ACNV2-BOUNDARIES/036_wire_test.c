/* Byte-exact check of 036 (run by v4Tests/scripts/runWireTests.sh, also with -wordSize 4): 64-bit ACN integer
   fields. With -wordSize 4 the C encoder wrote 0x00 in the upper 4 bytes of a negative two's complement value, and
   the decoders skipped the upper 4 bytes, so a value that does not fit in 32 bits was decoded as its low 32 bits. */
#include <assert.h>
#include <string.h>

#include "036.h"

static const byte minus2_one[32] = {
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE,     /* sb = -2, big endian */
    0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,     /* sl = -2, little endian */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,     /* ub = 1, big endian */
    0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00      /* ul = 1, little endian */
};

static flag decode(const byte* data)
{
    byte buf[MyPDU_REQUIRED_BYTES_FOR_ACN_ENCODING];
    BitStream stream;
    MyPDU out;
    int error = 0;

    memcpy(buf, data, sizeof buf);
    MyPDU_Initialize(&out);
    BitStream_AttachBuffer(&stream, buf, sizeof buf);
    return MyPDU_ACN_Decode(&out, &stream, &error);
}

/* the field at byte offset 'at' with its byte 'pos' replaced by 'v' must be rejected */
static void rejected(int at, int pos, byte v)
{
    byte data[32];
    memcpy(data, minus2_one, sizeof data);
    data[at + pos] = v;
    assert(!decode(data));
}

int main(void)
{
    byte encoded[MyPDU_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    MyPDU in, out;
    int error = 0;

    assert(sizeof encoded == sizeof minus2_one);
    in.sb = -2;
    in.sl = -2;
    in.ub = 1;
    in.ul = 1;
    BitStream_Init(&stream, encoded, sizeof encoded);
    assert(MyPDU_ACN_Encode(&in, &stream, &error, TRUE));
    assert(BitStream_GetLength(&stream) == 32 && memcmp(encoded, minus2_one, sizeof encoded) == 0);
    MyPDU_Initialize(&out);
    BitStream_AttachBuffer(&stream, encoded, sizeof encoded);
    assert(MyPDU_ACN_Decode(&out, &stream, &error));
    assert(MyPDU_Equal(&in, &out));

    rejected(0, 0, 0x00);     /* sb: 0x00FFFFFF FFFFFFFE does not fit in 32 bits */
    rejected(0, 3, 0x7F);
    rejected(8, 7, 0x00);     /* sl */
    rejected(16, 3, 0x01);    /* ub = 2^32 + 1 */
    rejected(24, 4, 0x01);    /* ul */
    return 0;
}
