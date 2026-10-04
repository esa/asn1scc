#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sample1.h"

/* Named bit N of a BIT STRING lives in arr[N / 8], mask 0x80 >> (N % 8) (X.690 leading bit = MSB).
   Runs before main(): a wrong helper aborts the automatic test program.
   The test runners generate the code with -typePrefix ASN1SCC_. */

#define CHECK(cond) do { if (!(cond)) { fprintf(stderr, "named bit check failed: %s (line %d)\n", #cond, __LINE__); abort(); } } while (0)

#define CHECK_BIT(T, name, N)                                                          \
    do {                                                                               \
        ASN1SCC_##T v;                                                                 \
        ASN1SCC_##T expected;                                                          \
        memset(&v, 0, sizeof(v));                                                      \
        memset(&expected, 0, sizeof(expected));                                        \
        CHECK(!ASN1SCC_##T##_has_##name(&v));                                          \
        ASN1SCC_##T##_set_##name(&v);                                                  \
        expected.arr[(N) / 8] = (byte)(0x80 >> ((N) % 8));                             \
        CHECK(memcmp(v.arr, expected.arr, sizeof(v.arr)) == 0);                        \
        CHECK(ASN1SCC_##T##_has_##name(&v));                                           \
        memset(v.arr, 0xFF, sizeof(v.arr));                                            \
        ASN1SCC_##T##_clear_##name(&v);                                                \
        memset(expected.arr, 0xFF, sizeof(expected.arr));                              \
        expected.arr[(N) / 8] = (byte)~(0x80 >> ((N) % 8));                            \
        CHECK(memcmp(v.arr, expected.arr, sizeof(v.arr)) == 0);                        \
        CHECK(!ASN1SCC_##T##_has_##name(&v));                                          \
    } while (0)

__attribute__((constructor)) static void named_bits_check(void)
{
    CHECK_BIT(NamedBits, bit0, 0);
    CHECK_BIT(NamedBits, bit1, 1);
    CHECK_BIT(NamedBits, bit2, 2);
    CHECK_BIT(NamedBits, bit3, 3);
    CHECK_BIT(NamedBits, bit4, 4);
    CHECK_BIT(NamedBits, bit5, 5);
    CHECK_BIT(NamedBits, bit6, 6);
    CHECK_BIT(NamedBits, bit7, 7);
    CHECK_BIT(NamedBits, bit9, 9);
    CHECK_BIT(NamedBits, bit70, 70);
    CHECK_BIT(FixedBits, first, 0);
    CHECK_BIT(FixedBits, last, 15);
}
