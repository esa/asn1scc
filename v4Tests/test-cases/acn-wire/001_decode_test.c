/* ACN INTEGER as ASCII digits with a terminator (run by v4Tests/scripts/runWireTests.sh): a value outside the range
   of the type, or a digit string beyond the integer type, is rejected with an error code (the same streams as
   001_decode_test.adb, x86 values). */
#include <stdio.h>
#include <string.h>
#include "001.h"

static int failures = 0;

#define CHECK(T, text, expected, value)                                                          \
    do {                                                                                         \
        byte buf[T##_REQUIRED_BYTES_FOR_ACN_ENCODING + 8];                                       \
        BitStream bs;                                                                            \
        T v;                                                                                     \
        int err = 0;                                                                             \
        flag ok;                                                                                 \
        memset(buf, 0, sizeof(buf));                                                             \
        memcpy(buf, text, strlen(text));                                                         \
        BitStream_AttachBuffer(&bs, buf, sizeof(buf));                                           \
        ok = T##_ACN_Decode(&v, &bs, &err);                                                      \
        if (ok != (expected) || (!ok && err == 0) || (ok && v != (T)(value))) {                  \
            printf("%s: unexpected result for %s\n", #T, text);                                  \
            failures++;                                                                          \
        }                                                                                        \
    } while (0)

int main(void)
{
    CHECK(Un, "0", TRUE, 0);
    CHECK(Un, "255", TRUE, 255);
    CHECK(Un, "256", FALSE, 0);
    CHECK(Un, "999", FALSE, 0);
    CHECK(Nr, "10", TRUE, 10);
    CHECK(Nr, "20", TRUE, 20);
    CHECK(Nr, "9", FALSE, 0);
    CHECK(Nr, "21", FALSE, 0);
    CHECK(Sg, "+100", TRUE, 100);
    CHECK(Sg, "-100", TRUE, -100);
    CHECK(Sg, "+101", FALSE, 0);
    CHECK(Sg, "-101", FALSE, 0);
    CHECK(Sg, "100", FALSE, 0);
    CHECK(Big, "18446744073709551615", TRUE, 18446744073709551615ULL);
    CHECK(Big, "18446744073709551616", FALSE, 0);
    CHECK(Big, "18446744073709551619", FALSE, 0);
    CHECK(Big, "99999999999999999999", FALSE, 0);
    CHECK(Any, "+9223372036854775807", TRUE, 9223372036854775807LL);
    CHECK(Any, "-9223372036854775808", TRUE, -9223372036854775807LL - 1);
    CHECK(Any, "-9223372036854775809", FALSE, 0);
    CHECK(Any, "+9223372036854775808", FALSE, 0);
    return failures == 0 ? 0 : 1;
}
