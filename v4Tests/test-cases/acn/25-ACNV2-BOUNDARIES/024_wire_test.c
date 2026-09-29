#define ACN_ONLY
/* Every short input must fail with an error. Complete input must decode
 * unchanged with error zero, both standalone and inside a SEQUENCE. */
#include <stdio.h>
#include <string.h>
#include "024.h"

#define CHECK(T, DECODE, BYTES) do {                                      \
    T value;                                                              \
    BitStream stream;                                                     \
    int error = 0;                                                        \
    T##_Initialize(&value);                                               \
    BitStream_AttachBuffer(&stream, size ? data : NULL, size);            \
    flag decoded = DECODE(&value, &stream, &error);                       \
    int ok = size < 16 ? !decoded && error != 0                           \
                       : decoded && error == 0 &&                         \
                         BitStream_GetLength(&stream) == 16 &&            \
                         memcmp((BYTES), data, 16) == 0;                  \
    if (!ok) {                                                            \
        fprintf(stderr, #DECODE ": %ld bytes: decoded=%d error=%d\n",     \
                size, decoded, error);                                    \
        passed = 0;                                                       \
    }                                                                     \
} while (0)

int main(void) {
    unsigned char data[16];
    int passed = 1;
    for (int i = 0; i < 16; ++i)
        data[i] = (unsigned char)i;
    for (long size = 0; size <= 16; ++size) {
        CHECK(UuidId, UuidId_ACN_Decode, value.arr);
        CHECK(Envelope, Envelope_ACN_Decode, value.id.arr);
#ifndef ACN_ONLY
        CHECK(UuidId, UuidId_Decode, value.arr);
        CHECK(Envelope, Envelope_Decode, value.id.arr);
#endif
    }
    return passed ? 0 : 1;
}
