/* Encoders into buffers that are too short, with a determinant shared by several consumers (coverage cycle 15k; run by
   v4Tests/scripts/runWireTests.sh, --acn-v2): TS = a, n (8-bit length field), pl <n> (two SEQUENCE OF elements, each an
   OCTET STRING (CONTAINING Cont) region of n octets), tail. The --acn-v2 encoder writes each region and patches n with
   its length (from positions); the second patch must agree with the first.
   When the contents of the second element do not fit, the writes are refused but the encoder goes on: the region length
   it computes from positions differs from the first one's, and the shared-determinant check fails with its own error
   code (ERR_ACN_ENCODE_TS_PL_ITEMS_ELM_C_2), not ERR_BITSTREAM_OVERFLOW. The overflow member of the BitStream is set in
   every such row: it is the test for "the buffer was too short".
   Rows: full buffer (TRUE, code 0, 89 bits, bytes pinned); every shorter buffer from start bits 0 .. 7 between guard
   bytes (FALSE, a nonzero code, overflow set, position inside, guards intact); pinned rows with the determinant's code
   and with ERR_BITSTREAM_OVERFLOW. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "023.h"

/* ERR_ACN_ENCODE_TS_PL_ITEMS_ELM_C_2: the shared-determinant check of the region length, defined in 023.c */
#define ERR_SHARED_LENGTH 86

#define GUARD 8
#define GUARD_BYTE 0xA5

static int failures = 0;

typedef struct {
    flag ok;
    int err;
    flag overflow;
    long pos;
    int guardsOk;
    byte data[32];
} Outcome;

static Outcome encode_into(const TS* v, long k, int startBit)
{
    Outcome o;
    byte* base = (byte*)malloc((size_t)(k + 2 * GUARD));
    BitStream bs;
    long i;
    memset(&o, 0, sizeof(o));
    if (base == NULL)
        exit(2);
    memset(base, GUARD_BYTE, (size_t)(k + 2 * GUARD));
    BitStream_Init(&bs, base + GUARD, k);
    bs.currentBit = startBit;
    o.ok = TS_ACN_Encode(v, &bs, &o.err, TRUE);
    o.overflow = bs.overflow;
    o.pos = bs.currentByte * 8L + bs.currentBit;
    o.guardsOk = 1;
    for (i = 0; i < GUARD; i++) {
        if (base[i] != GUARD_BYTE || base[GUARD + k + i] != GUARD_BYTE)
            o.guardsOk = 0;
    }
    memcpy(o.data, base + GUARD, (size_t)(k < 32 ? k : 32));
    free(base);
    return o;
}

static void report(const char* row, long k, int s, const Outcome* o)
{
    printf("TS %s k=%ld s=%d: result %d, error code %d, overflow %d, position %ld, guards %s\n", row, k, s, (int)o->ok,
           o->err, (int)o->overflow, o->pos, o->guardsOk ? "intact" : "OVERWRITTEN");
    failures++;
}

#if defined(ASN1SCC_CHECKED_WRITES)
static void check_row(const TS* v, long k, int s, int expectedErr, long expectedPos)
{
    Outcome o = encode_into(v, k, s);
    if (o.ok || o.err != expectedErr || !o.overflow || o.pos != expectedPos || !o.guardsOk)
        report("pinned", k, s, &o);
}
#endif

int main(void)
{
    static TS t;
    static const byte expected[] = { 0x11, 0x04, 0x80, 0xC0, 0xE1, 0x00, 0x01, 0x40, 0xE1, 0x00, 0x11, 0x00 };
    const long nBits = 89;
    long k;
    int s;
    int i;
    Outcome o;

    t.a = 0x11;
    t.tail = 0x22;
    t.pl.items.nCount = 2;
    for (i = 0; i < 2; i++) {
        t.pl.items.arr[i].c.v = (asn1SccUint)(i + 1);
        t.pl.items.arr[i].c.w.nCount = 2;
        t.pl.items.arr[i].c.w.arr[0] = 7;
        t.pl.items.arr[i].c.w.arr[1] = 8;
    }

    o = encode_into(&t, (nBits + 7) / 8, 0);
    if (!o.ok || o.err != 0 || o.overflow || o.pos != nBits || !o.guardsOk ||
        memcmp(o.data, expected, sizeof(expected)) != 0)
        report("full", (nBits + 7) / 8, 0, &o);

#if defined(ASN1SCC_CHECKED_WRITES)
    for (s = 0; s < 8; s++) {
        long nFrom = (s + nBits + 7) / 8;
        for (k = 0; k < nFrom; k++) {
            if (k == 0 && s != 0)
                continue;
            o = encode_into(&t, k, s);
            if (o.ok || o.err == 0 || !o.overflow || o.pos > k * 8 || o.pos < 0 || !o.guardsOk)
                report("short", k, s, &o);
        }
    }
    /* the contents of the second element are refused: the second region length disagrees with the first one */
    check_row(&t, 8, 0, ERR_SHARED_LENGTH, 64);
    check_row(&t, 9, 3, ERR_SHARED_LENGTH, 70);
    /* a and n fit, the regions do not: no element writes its contents, so no length disagrees with another */
    check_row(&t, 3, 0, ERR_BITSTREAM_OVERFLOW, 24);
#else
    (void)k;
    (void)s;
#endif

    if (failures != 0) {
        printf("023: %d failures\n", failures);
        return 1;
    }
    printf("023 OK\n");
    return 0;
}
