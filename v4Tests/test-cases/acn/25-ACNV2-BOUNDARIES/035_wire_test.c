/* Byte-exact check of 035 (run by v4Tests/scripts/runWireTests.sh, with and without --acn-v2): 'size deduced'
   contents of an OCTET STRING CONTAINING region that does not start at a byte boundary. The region is whole octets
   from its first bit, so it ends inside a byte; the decoder must count the elements up to that bit. One 9-bit element
   and its 7-bit padding leave, up to the end of the last byte, room for a second one: the --acn-v2 decoder took the
   byte after the region as its end and decoded two elements. */
#include <assert.h>
#include <string.h>

#include "035.h"

static void checkExt(const ExtPdu* in, const byte* expected, long expectedLength)
{
    byte encoded[ExtPdu_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    ExtPdu out;
    long length;
    int error = 0;

    BitStream_Init(&stream, encoded, sizeof encoded);
    assert(ExtPdu_ACN_Encode(in, &stream, &error, TRUE));
    length = BitStream_GetLength(&stream);
    assert(length == expectedLength && memcmp(encoded, expected, (size_t)expectedLength) == 0);
    ExtPdu_Initialize(&out);
    BitStream_AttachBuffer(&stream, encoded, length);
    assert(ExtPdu_ACN_Decode(&out, &stream, &error));
    assert(out.payload.nCount == in->payload.nCount);
    assert(ExtPdu_Equal(in, &out));
}

static void checkEmb(const EmbPdu* in, const byte* expected, long expectedLength)
{
    byte encoded[EmbPdu_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    EmbPdu out;
    long length;
    int error = 0;

    BitStream_Init(&stream, encoded, sizeof encoded);
    assert(EmbPdu_ACN_Encode(in, &stream, &error, TRUE));
    length = BitStream_GetLength(&stream);
    assert(length == expectedLength && memcmp(encoded, expected, (size_t)expectedLength) == 0);
    EmbPdu_Initialize(&out);
    BitStream_AttachBuffer(&stream, encoded, length);
    assert(EmbPdu_ACN_Decode(&out, &stream, &error));
    assert(out.payload.nCount == in->payload.nCount);
    assert(EmbPdu_Equal(in, &out));
}

int main(void)
{
    ExtPdu ext;
    EmbPdu emb;

    /* hdr 0101 | len 2 (16 bits) | 300 = 1 0010 1100, 7 zero bits | tail 1010 0101 | 4 zero bits */
    static const byte extBytes[] = { 0x50, 0x00, 0x29, 0x60, 0x0A, 0x50 };
    /* hdr 0101 | length 2 - 2 (7 bits) | 300, 7 zero bits | tail 1010 0101 | 5 zero bits */
    static const byte embBytes[] = { 0x50, 0x12, 0xC0, 0x14, 0xA0 };

    ExtPdu_Initialize(&ext);
    ext.hdr = 5;
    ext.payload.nCount = 1;
    ext.payload.arr[0] = 300;
    ext.tail = 165;
    checkExt(&ext, extBytes, (long)sizeof extBytes);

    EmbPdu_Initialize(&emb);
    emb.hdr = 5;
    emb.payload.nCount = 1;
    emb.payload.arr[0] = 300;
    emb.tail = 165;
    checkEmb(&emb, embBytes, (long)sizeof embBytes);
    return 0;
}
