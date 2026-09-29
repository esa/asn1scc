#include <stdio.h>
#include <string.h>
#include "023.h"

/* Decodes `size` bytes of `input` (none: a null buffer) as a Mask. */
static int decode_mask(const unsigned char *input, long size, Mask *value, int *error) {
    unsigned char buffer[3];
    BitStream stream;

    memcpy(buffer, input, (size_t)size);
    memset(value, 0, sizeof *value);
    BitStream_AttachBuffer(&stream, size > 0 ? buffer : NULL, size);
    return Mask_ACN_Decode(value, &stream, error);
}

static int decode_bits(const unsigned char *input, long size, Bits *value, int *error) {
    unsigned char buffer[3];
    BitStream stream;

    memcpy(buffer, input, (size_t)size);
    memset(value, 0, sizeof *value);
    BitStream_AttachBuffer(&stream, size > 0 ? buffer : NULL, size);
    return Bits_ACN_Decode(value, &stream, error);
}

/* A failed decode: the length (15, above SIZE (0..8), or no input) or the
 * contents (short) do not decode. */
static int rejected(const char *what, const unsigned char *input, long size, int expected) {
    Mask mask;
    Bits bits;
    int error = 0;
    flag decoded = expected == ERR_ACN_DECODE_MASK ? decode_mask(input, size, &mask, &error)
                                                   : decode_bits(input, size, &bits, &error);
    if (decoded || error != expected) {
        fprintf(stderr, "%s: decoded=%d error=%d, expected error %d\n", what, decoded, error,
                expected);
        return 0;
    }
    return 1;
}

int main(void) {
    static const unsigned char mask_bytes[3] = {0x2A, 0xAB, 0xB0};   /* length 2: AA BB */
    static const unsigned char bits_bytes[1] = {0x4A};               /* length 4: 1010 */
    static const unsigned char length_15[1] = {0xF0};
    unsigned char buffer[3];
    BitStream stream;
    Mask mask = {2, {0xAA, 0xBB}}, mask_back;
    Bits bits = {4, {0xA0}}, bits_back;
    int error = 0, passed = 1;

    BitStream_Init(&stream, buffer, sizeof buffer);
    if (!Mask_ACN_Encode(&mask, &stream, &error, TRUE) || BitStream_GetLength(&stream) != 3 ||
        memcmp(buffer, mask_bytes, 3) != 0 || !decode_mask(mask_bytes, 3, &mask_back, &error) ||
        error != 0 || !Mask_Equal(&mask, &mask_back)) {
        fprintf(stderr, "Mask: no round trip through 2A AB B0\n");
        passed = 0;
    }
    BitStream_Init(&stream, buffer, sizeof buffer);
    if (!Bits_ACN_Encode(&bits, &stream, &error, TRUE) || BitStream_GetLength(&stream) != 1 ||
        buffer[0] != bits_bytes[0] || !decode_bits(bits_bytes, 1, &bits_back, &error) ||
        error != 0 || !Bits_Equal(&bits, &bits_back)) {
        fprintf(stderr, "Bits: no round trip through 4A\n");
        passed = 0;
    }

    passed &= rejected("Mask, length 15", length_15, 1, ERR_ACN_DECODE_MASK);
    passed &= rejected("Mask, no input", length_15, 0, ERR_ACN_DECODE_MASK);
    passed &= rejected("Mask, short contents", mask_bytes, 2, ERR_ACN_DECODE_MASK);
    passed &= rejected("Bits, length 15", length_15, 1, ERR_ACN_DECODE_BITS);
    passed &= rejected("Bits, no input", length_15, 0, ERR_ACN_DECODE_BITS);
    return passed ? 0 : 1;
}
