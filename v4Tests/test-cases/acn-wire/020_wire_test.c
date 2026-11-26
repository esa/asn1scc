/* The final validity check of a C decoder rejects a value that the decoding itself accepts (run by
   v4Tests/scripts/runWireTests.sh): a null-terminated ACN IA5String with SIZE (3 .. 8) whose terminator comes after
   fewer than 3 characters is read successfully, and only the validator called at the end of the decoder (strlen below
   the minimum) turns the result into FALSE with the constraint's error code. Each row pins the result, the error code
   and the stream position (in bits) after the call.
   - Name: the type itself (Name_ACN_Decode's own check, ERR_NAME);
   - Rec: a component that references Name (Name_ACN_Decode, called by Rec_ACN_Decode, rejects first);
   - Msg: an inline component, decoded inline without a check: only Msg_ACN_Decode's own final check rejects
     (ERR_MSG_TEXT);
   - a decode that fails (8 characters and no terminator) returns its own error code: the validator is called only
     after a successful decode. */
#include <stdio.h>
#include <string.h>
#include "020.h"

static int failures = 0;

static void report(const char* type, int row, flag ok, int err, long pos)
{
    printf("%s row %d: unexpected result %d / error code %d / position %ld\n", type, row, (int)ok, err, pos);
    failures++;
}

static long position(const BitStream* bs)
{
    return bs->currentByte * 8L + bs->currentBit;
}

static void check_name(int row, const char* bytes, size_t n, flag expected, int expectedErr, long expectedPos,
                       const char* value)
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
    if (ok != expected || err != expectedErr || position(&bs) != expectedPos || (ok && strcmp(v, value) != 0))
        report("Name", row, ok, err, position(&bs));
}

static void check_rec(int row, const char* bytes, size_t n, flag expected, int expectedErr, long expectedPos)
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
    if (ok != expected || err != expectedErr || position(&bs) != expectedPos)
        report("Rec", row, ok, err, position(&bs));
}

static void check_msg(int row, const char* bytes, size_t n, flag expected, int expectedErr, long expectedPos)
{
    byte buf[Msg_REQUIRED_BYTES_FOR_ACN_ENCODING + 8];
    BitStream bs;
    Msg v;
    int err = -1;
    flag ok;
    memset(buf, 0xFF, sizeof(buf));
    memcpy(buf, bytes, n);
    memset(&v, 'x', sizeof(v));
    BitStream_AttachBuffer(&bs, buf, sizeof(buf));
    ok = Msg_ACN_Decode(&v, &bs, &err);
    if (ok != expected || err != expectedErr || position(&bs) != expectedPos)
        report("Msg", row, ok, err, position(&bs));
}

int main(void)
{
    /* valid: decoded and accepted, position after the terminator */
    check_name(1, "abc\0", 4, TRUE, 0, 32, "abc");
    check_name(2, "abcdefgh\0", 9, TRUE, 0, 72, "abcdefgh");
    /* decoded without error, rejected by the final validity check, position after the terminator */
    check_name(3, "ab\0", 3, FALSE, ERR_NAME, 24, NULL);
    check_name(4, "a\0", 2, FALSE, ERR_NAME, 16, NULL);
    check_name(5, "\0", 1, FALSE, ERR_NAME, 8, NULL);
    /* the decoding fails (no terminator after 8 characters): its own code, the validator is not called */
    check_name(6, "abcdefghi", 9, FALSE, ERR_ACN_DECODE_NAME, 72, NULL);
    check_rec(1, "\x05" "abc\0", 5, TRUE, 0, 40);
    check_rec(2, "\x05" "ab\0", 4, FALSE, ERR_NAME, 32);
    check_rec(3, "\x05" "\0", 2, FALSE, ERR_NAME, 16);
    check_msg(1, "\x05" "abc\0", 5, TRUE, 0, 40);
    check_msg(2, "\x05" "ab\0", 4, FALSE, ERR_MSG_TEXT, 32);
    check_msg(3, "\x05" "\0", 2, FALSE, ERR_MSG_TEXT, 16);
    check_msg(4, "\x05" "abcdefghi", 10, FALSE, ERR_ACN_DECODE_MSG_TEXT, 80);
    return failures == 0 ? 0 : 1;
}
