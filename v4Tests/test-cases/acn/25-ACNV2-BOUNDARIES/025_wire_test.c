/* Cover rejected determinants, truncated contents, and valid boundaries. */
#include <stdio.h>
#include <string.h>
#include "025.h"

#define CHECK(T, MIN, MAX, COUNT) do {                                        \
    T value;                                                                  \
    BitStream stream;                                                         \
    int error = 0;                                                            \
    T##_Initialize(&value);                                                   \
    BitStream_AttachBuffer(&stream, size ? data : NULL, size);                \
    flag decoded = T##_ACN_Decode(&value, &stream, &error);                   \
    int expected = length >= (MIN) && length <= (MAX) && size >= 1 + length;  \
    int ok = expected ? decoded && error == 0 && (COUNT) == length &&         \
                        BitStream_GetLength(&stream) == 1 + length &&         \
                        memcmp(value.data.arr, data + 1, (size_t)length) == 0 \
                      : !decoded && error != 0;                               \
    if (!ok) {                                                                \
        fprintf(stderr, #T ": len=%d size=%ld decoded=%d error=%d\n",         \
                length, size, decoded, error);                                \
        passed = 0;                                                           \
    }                                                                         \
} while (0)

int main(void) {
    static const unsigned char lengths[] = {0, 1, 2, 8, 9, 255};
    unsigned char data[9] = {0, 1, 2, 3, 4, 5, 6, 7, 8};
    int passed = 1;
    for (size_t i = 0; i < sizeof lengths; ++i) {
        int length = lengths[i];
        data[0] = lengths[i];
        for (long size = 0; size <= 9; ++size) {
            CHECK(Bounded, 1, 8, value.data.nCount);
            CHECK(ZeroMin, 0, 8, value.data.nCount);
            CHECK(Fixed, 2, 2, 2);
        }
    }
    return passed ? 0 : 1;
}
