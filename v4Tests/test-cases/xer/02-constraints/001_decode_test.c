/* XER decoding of values outside their constraints (run by v4Tests/scripts/runWireTests.sh): the decoder must fail.
   Before, the XER decoders checked only some INTEGER ranges and accepted an empty s, a 1-octet o or an empty l. */
#include <assert.h>
#include <string.h>

#include "001.h"

static flag decode(const char* xml)
{
    static byte buf[4096];
    ByteStream stream;
    Msg out;
    int error = 0;

    memset(buf, 0, sizeof buf);
    memcpy(buf, xml, strlen(xml));
    Msg_Initialize(&out);
    ByteStream_AttachBuffer(&stream, buf, (long)strlen(xml));
    return Msg_XER_Decode(&out, &stream, &error);
}

int main(void)
{
    assert(decode("<Msg><n>5</n><s>abc</s><o>ABCD</o><l><Msg-l-elem>1</Msg-l-elem></l></Msg>"));
    assert(!decode("<Msg><n>11</n><s>abc</s><o>ABCD</o><l><Msg-l-elem>1</Msg-l-elem></l></Msg>"));
    assert(!decode("<Msg><n>5</n><s></s><o>ABCD</o><l><Msg-l-elem>1</Msg-l-elem></l></Msg>"));
    assert(!decode("<Msg><n>5</n><s>abc</s><o>AB</o><l><Msg-l-elem>1</Msg-l-elem></l></Msg>"));
    assert(!decode("<Msg><n>5</n><s>abc</s><o>ABCD</o><l></l></Msg>"));
    assert(!decode("<Msg><n>5</n><s>abc</s><o>ABCD</o><l><Msg-l-elem>8</Msg-l-elem></l></Msg>"));
    return 0;
}
