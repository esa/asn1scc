/* Byte-exact check of 018 (run by v4Tests/scripts/runWireTests.sh), with
   the Reader Requirements box contents of ITU-T T.801 M.11.1: ML, FUAM,
   DCM, NSF, NSF pairs (SF, SM), NVF. ML comes from the masks, with or
   without flags, and must be the same for all of them. */
#include <assert.h>
#include <string.h>

#include "018.h"

static int encode(const Requirements *input, byte *encoded, size_t size, long *length)
{
    BitStream stream;
    int error = 0;
    flag ok;
    BitStream_Init(&stream, encoded, (long)size);
    ok = Requirements_ACN_Encode(input, &stream, &error, TRUE);
    *length = BitStream_GetLength(&stream);
    return ok ? 0 : error;
}

static void round_trip(const Requirements *input, const byte *expected, long size)
{
    byte encoded[Requirements_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    BitStream stream;
    Requirements output;
    long length;
    int error = 0;
    assert(encode(input, encoded, sizeof encoded, &length) == 0);
    assert(length == size && memcmp(encoded, expected, (size_t)size) == 0);
    Requirements_Initialize(&output);
    BitStream_AttachBuffer(&stream, encoded, length);
    assert(Requirements_ACN_Decode(&output, &stream, &error));
    assert(Requirements_Equal(input, &output));
}

int main(void)
{
    /* ML 1; features 1 and 2, needed to understand and to display. */
    static const byte two_flags[] = {1, 0xC0, 0xC0, 0, 2, 0, 1, 0x80, 0, 2, 0x40, 0, 0};
    static const byte no_flags[] = {2, 0x80, 0x00, 0x80, 0x00, 0, 0, 0, 0};
    byte encoded[Requirements_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};
    Requirements input;
    long length;

    Requirements_Initialize(&input);
    input.fuam.nCount = 1;
    input.fuam.arr[0] = 0xC0;
    input.dcm.nCount = 1;
    input.dcm.arr[0] = 0xC0;
    input.standard.nCount = 2;
    input.standard.arr[0].sf = 1;
    input.standard.arr[0].sm.nCount = 1;
    input.standard.arr[0].sm.arr[0] = 0x80;
    input.standard.arr[1].sf = 2;
    input.standard.arr[1].sm.nCount = 1;
    input.standard.arr[1].sm.arr[0] = 0x40;
    round_trip(&input, two_flags, sizeof two_flags);

    /* No flags at all: ML 2 from FUAM and DCM alone. */
    input.fuam.nCount = input.dcm.nCount = 2;
    input.fuam.arr[0] = input.dcm.arr[0] = 0x80;
    input.fuam.arr[1] = input.dcm.arr[1] = 0x00;
    input.standard.nCount = 0;
    round_trip(&input, no_flags, sizeof no_flags);

    /* A DCM longer than the FUAM, with and without flags. */
    input.dcm.nCount = 3;
    assert(encode(&input, encoded, sizeof encoded, &length) == ERR_ACN_DET_CONSISTENCY_MISMATCH);
    input.fuam.nCount = input.dcm.nCount = 1;
    input.standard.nCount = 1;
    input.standard.arr[0].sm.nCount = 2;
    assert(encode(&input, encoded, sizeof encoded, &length) == ERR_ACN_DET_CONSISTENCY_MISMATCH);

    return 0;
}
