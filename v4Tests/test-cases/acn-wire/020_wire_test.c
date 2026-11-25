/* The final validity check of a C decoder rejects a value that the decoding itself accepts (run by
   v4Tests/scripts/runWireTests.sh): a null-terminated ACN IA5String with SIZE (3 .. 8) whose terminator comes after
   fewer than 3 characters is read successfully, and only Name_IsConstraintValid (strlen below the minimum) turns the
   result into FALSE with ERR_NAME. Also through a SEQUENCE component (Rec.label). */
#include <stdio.h>
#include <string.h>
#include "020.h"

static int failures = 0;

static void check_name(const char* bytes, size_t n, flag expected, int expectedErr, const char* value)
{
    byte buf[Name_REQUIRED_BYTES_FOR_ACN_ENCODING + 8];
    BitStream bs;
    Name v;
    int err = -1;
    flag ok;
    memset(buf, 0xFF, sizeof(buf));
    memcpy(buf, bytes, n);
    memset(v, 'x', sizeof(v));
    BitStream_AttachBuffer(&bs, buf, sizeof(buf));
    ok = Name_ACN_Decode(v, &bs, &err);
    if (ok != expected || err != expectedErr || (ok && strcmp(v, value) != 0)) {
        printf("Name: unexpected result %d / %d for a stream of %zu bytes\n", (int)ok, err, n);
        failures++;
    }
}

static void check_rec(const char* bytes, size_t n, flag expected, int expectedErr)
{
    byte buf[Rec_REQUIRED_BYTES_FOR_ACN_ENCODING + 8];
    BitStream bs;
    Rec v;
    int err = -1;
    flag ok;
    memset(buf, 0xFF, sizeof(buf));
    memcpy(buf, bytes, n);
    memset(&v, 'x', sizeof(v));
    BitStream_AttachBuffer(&bs, buf, sizeof(buf));
    ok = Rec_ACN_Decode(&v, &bs, &err);
    if (ok != expected || err != expectedErr) {
        printf("Rec: unexpected result %d / %d for a stream of %zu bytes\n", (int)ok, err, n);
        failures++;
    }
}

int main(void)
{
    check_name("abc\0", 4, TRUE, 0, "abc");
    check_name("abcdefgh\0", 9, TRUE, 0, "abcdefgh");
    /* decoded without error, rejected by the validator */
    check_name("ab\0", 3, FALSE, ERR_NAME, NULL);
    check_name("a\0", 2, FALSE, ERR_NAME, NULL);
    check_name("\0", 1, FALSE, ERR_NAME, NULL);
    check_rec("\x05" "abc\0", 5, TRUE, 0);
    check_rec("\x05" "ab\0", 4, FALSE, ERR_NAME);
    check_rec("\x05" "\0", 2, FALSE, ERR_NAME);
    return failures == 0 ? 0 : 1;
}
