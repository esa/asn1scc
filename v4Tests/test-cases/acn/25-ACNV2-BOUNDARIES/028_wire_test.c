/* Byte-exact check of 028 (run by v4Tests/scripts/runWireTests.sh): two ACN
   fields with the same name, consumed by c1 and c2 with different values
   (yes = 1, no = 2). One shared AcnInsertedFieldRef (q/issues.md Issue 52)
   failed the encoding and decoded c1 with c2's value; the automatic test
   values never select different alternatives in c1 and c2. */
#include <assert.h>

#include "028.h"

#define CHECK(T, SET, B0, B1) do {                                  \
    byte encoded[T##_REQUIRED_BYTES_FOR_ACN_ENCODING] = {0};         \
    BitStream stream;                                                \
    T input;                                                         \
    T output;                                                        \
    int error = 0;                                                   \
    T##_Initialize(&input);                                          \
    input.a = 5;                                                     \
    input.c1.kind = yes_PRESENT;                                     \
    input.c2.kind = no_PRESENT;                                      \
    SET;                                                             \
    BitStream_Init(&stream, encoded, sizeof encoded);                \
    assert(T##_ACN_Encode(&input, &stream, &error, TRUE));           \
    assert(error == 0);                                              \
    assert(encoded[0] == (B0));                                      \
    assert(encoded[1] == (B1));                                      \
    T##_Initialize(&output);                                         \
    BitStream_AttachBuffer(&stream, encoded, sizeof encoded);        \
    assert(T##_ACN_Decode(&output, &stream, &error));                \
    assert(T##_Equal(&input, &output));                              \
} while (0)

int main(void)
{
    /* a = 0101, header.detail = 001, header.b = 011, trailer.detail = 010, trailer.b = 100 */
    CHECK(S1, (input.header.b = 3, input.trailer.b = 4), 0x52, 0xD4);
    /* a = 0101, detail = 001, header.detail = 010, header.b = 110 */
    CHECK(S2, input.header.b = 6, 0x52, 0xB0);
    /* a = 0101, header.sub.detail = 001, header.sub.b = 110, header.detail = 010 */
    CHECK(S3, input.header.sub.b = 6, 0x53, 0x90);
    /* a = 0101, header.detail = 001, header.b = 011, trailer.detail = 010, trailer.b = 100 */
    CHECK(S4, (input.header.b = 3, input.trailer.b = 4), 0x52, 0xD4);
    return 0;
}
