/* q/issues.md Issue 85: the C twin of generated_tests.adb (the C decoders already rejected a missing terminator). */
#include <stdio.h>
#include <string.h>
#include "a.h"

static int failures = 0;

static void report(const char* name, int expectOk, flag ok, int err)
{
    int pass = expectOk ? (ok != 0) : (ok == 0 && err != 0);
    printf("%s %s: ok=%d err=%d\n", pass ? "PASS" : "FAIL", name, (int)ok, err);
    if (!pass) failures++;
}

#define DECODE(T, NAME, BYTES, LEN, EXPECT, VAL)                                    \
    do {                                                                            \
        byte buf[T##_REQUIRED_BYTES_FOR_ACN_ENCODING];                              \
        BitStream bs; int err = 0; flag ok;                                         \
        memset(buf, 0, sizeof buf);                                                 \
        memcpy(buf, BYTES, LEN);                                                    \
        BitStream_AttachBuffer(&bs, buf, sizeof buf);                               \
        ok = T##_ACN_Decode(VAL, &bs, &err);                                        \
        report(NAME, EXPECT, ok, err);                                              \
    } while (0)

int main(void)
{
    Str s; Str2 s2; NumT n; SNumT sn;
    const char* z20 = "00000000000000000000";
    char b[32];
    DECODE(Str, "Str, 5 characters + NUL", "ABCDE\0", 6, 1, s);
    DECODE(Str, "Str, 2 characters + NUL", "AB\0", 3, 1, s);
    DECODE(Str, "Str, 5 characters without terminator", "ABCDEX", 6, 0, s);
    DECODE(Str2, "Str2, 5 characters + 0D0A", "ABCDE\r\n", 7, 1, s2);
    DECODE(Str2, "Str2, 5 characters + 0D X", "ABCDE\rX", 7, 0, s2);
    DECODE(Str2, "Str2, 5 characters + X 0A", "ABCDEX\n", 7, 0, s2);
    DECODE(NumT, "NumT, 123 + NUL", "123\0", 4, 1, &n);
    memcpy(b, z20, 20); b[20] = 0;
    DECODE(NumT, "NumT, 20 digits + NUL", b, 21, 1, &n);
    b[20] = 'X';
    DECODE(NumT, "NumT, 20 digits without terminator", b, 21, 0, &n);
    DECODE(SNumT, "SNumT, -123 + NUL", "-123\0", 5, 1, &sn);
    b[0] = '+'; memcpy(b + 1, z20, 20); b[21] = 0;
    DECODE(SNumT, "SNumT, + 20 digits + NUL", b, 22, 1, &sn);
    b[21] = 'X';
    DECODE(SNumT, "SNumT, + 20 digits without terminator", b, 22, 0, &sn);
    if (failures) printf("FAILURES: %d\n", failures);
    return failures != 0;
}
