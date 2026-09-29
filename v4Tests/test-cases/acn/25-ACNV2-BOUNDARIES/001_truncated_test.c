/* Truncated input for 001 (run by v4Tests/scripts/runWireTests.sh with
   -fsanitize=bool): when reading the deferred determinant first.more fails,
   the decoder must not copy the temporary it decoded into, which the failed
   read never wrote. dirty_stack() leaves a byte that is not a valid flag
   where that temporary lives, so UBSan reports the read. */
#include <assert.h>
#include <string.h>

#include "001.h"

static void dirty_stack(void)
{
    volatile unsigned char junk[4096];
    memset((void *)junk, 0x4F, sizeof junk);
}

static void decode_fails(const byte *data, long size)
{
    static byte copy[2];
    BitStream stream;
    Chain output;
    int error = 0;

    memcpy(copy, data, (size_t)size);
    BitStream_AttachBuffer(&stream, copy, size);
    dirty_stack();
    assert(!Chain_ACN_Decode(&output, &stream, &error));
}

int main(void)
{
    static const byte more[1] = {0x80};   /* first.more = 1, then no bytes */

    decode_fails(more, 0);                /* first.more itself missing */
    decode_fails(more, 1);                /* second missing */
    return 0;
}
