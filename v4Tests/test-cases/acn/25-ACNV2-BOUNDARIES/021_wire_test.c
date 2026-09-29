/* Encodes Chain values after one byte FF already in the stream and
 * compares the bytes and the error code with the expected ones. Exit
 * status 0 means every case is right. */
#include <stdio.h>
#include <string.h>
#include "021.h"

static int run(const char *name, const Chain *v, const unsigned char *want, long nwant) {
    unsigned char buf[16];
    BitStream s;
    int err = 0, ok, right;
    long i, n;

    memset(buf, 0, sizeof buf);
    BitStream_AttachBuffer(&s, buf, sizeof buf);
    BitStream_EncodeConstraintWholeNumber(&s, 0xFF, 0, 255);
    ok = Chain_ACN_Encode(v, &s, &err, TRUE);
    n = BitStream_GetLength(&s);
    right = ok && err == 0 && n == nwant && memcmp(buf, want, (size_t)n) == 0;
    printf("%s: %s ret=%d err=%d bytes:", right ? "ok  " : "BAD ", name, ok, err);
    for (i = 0; i < n; i++) printf(" %02X", buf[i]);
    printf("   expected ret=1 err=0 bytes:");
    for (i = 0; i < nwant; i++) printf(" %02X", want[i]);
    printf("\n");
    return right;
}

static int reject_missing_producer(void) {
    unsigned char buf[16] = {0};
    BitStream stream;
    Chain value;
    int error = 0;
    int encoded;

    memset(&value, 0, sizeof value);
    value.exist.b2 = 1;
    BitStream_AttachBuffer(&stream, buf, sizeof buf);
    encoded = Chain_ACN_Encode(&value, &stream, &error, TRUE);
    if (encoded || error != ERR_ACN_DET_CONSISTENCY_MISMATCH) {
        fprintf(stderr, "missing producer: ret=%d err=%d\n", encoded, error);
        return 0;
    }
    return 1;
}

int main(void) {
    static const unsigned char one[] = {0xFF, 0x05};
    static const unsigned char two[] = {0xFF, 0x81, 0x48};
    static const unsigned char three[] = {0xFF, 0x81, 0x80, 0x05};
    static const unsigned char four[] = {0xFF, 0x81, 0x80, 0x80, 0x05};
    Chain v;
    int all = 1;

    memset(&v, 0, sizeof v);                 /* 1 byte: b0 */
    v.b0.bits = 5;
    all &= run("b0", &v, one, sizeof one);

    memset(&v, 0, sizeof v);                 /* 2 bytes: b0 b1 */
    v.b0.bits = 1;
    v.exist.b1 = 1;
    v.b1.bits = 0x48;
    all &= run("b0 b1", &v, two, sizeof two);

    memset(&v, 0, sizeof v);                 /* 3 bytes: b0 b1 b2 */
    v.b0.bits = 1;
    v.exist.b1 = 1;
    v.exist.b2 = 1;
    v.b2.bits = 5;
    all &= run("b0 b1 b2", &v, three, sizeof three);

    memset(&v, 0, sizeof v);                 /* 4 bytes: b0 b1 b2 b3 */
    v.b0.bits = 1;
    v.exist.b1 = 1;
    v.exist.b2 = 1;
    v.exist.b3 = 1;
    v.b3.bits = 5;
    all &= run("b0 b1 b2 b3", &v, four, sizeof four);

    all &= reject_missing_producer();
    return all ? 0 : 1;
}
