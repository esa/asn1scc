/* XER size of an ENUMERATED (GitHub #272, run by v4Tests/scripts/runWireTests.sh): the longest item must fit in a
   buffer of exactly <T>_REQUIRED_BYTES_FOR_XER_ENCODING bytes, alone, in a SEQUENCE and in a SEQUENCE OF. Before, the
   size came from the name that sorts last (zlib), and these encodings failed. */
#include <assert.h>
#include <string.h>

#include "003.h"

static byte buf[4096];

int main(void)
{
    ByteStream s;
    int e = 0;
    Compression_Type c = Compression_Type_alsoReservedForLaterUse;
    Msg m;
    List l;
    int i;

    m.c = Compression_Type_alsoReservedForLaterUse;
    for (i = 0; i < 5; i++)
        l.arr[i] = Compression_Type_alsoReservedForLaterUse;
    ByteStream_Init(&s, buf, Compression_Type_REQUIRED_BYTES_FOR_XER_ENCODING);
    assert(Compression_Type_XER_Encode(&c, &s, &e, TRUE));
    ByteStream_Init(&s, buf, Msg_REQUIRED_BYTES_FOR_XER_ENCODING);
    assert(Msg_XER_Encode(&m, &s, &e, TRUE));
    ByteStream_Init(&s, buf, List_REQUIRED_BYTES_FOR_XER_ENCODING);
    assert(List_XER_Encode(&l, &s, &e, TRUE));
    return 0;
}
