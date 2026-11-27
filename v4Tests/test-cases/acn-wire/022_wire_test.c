/* Encoders into buffers that are too short: bound-checked C runtime writes (coverage cycle 15k; run by
   v4Tests/scripts/runWireTests.sh, -uPER -ACN, legacy and --acn-v2, several builds).
   Every buffer of k bytes lies between two guard regions of 0xA5 bytes in one allocation; a row checks the result,
   the error code, the stream position (in bits) and the guards.
   - full buffer: TRUE, error code 0, the encoded bits and bytes pinned (the same wire as before the checks); from start
     bits 1 .. 7 a buffer of exactly the bytes needed gives the same end and bytes as a larger one;
   - every buffer shorter than the encoding, from start bit 0 .. 7: FALSE, ERR_BITSTREAM_OVERFLOW, position inside
     the buffer, guards intact, and the bits before the position equal those of the full encoding and no bit after
     it is set (the stream freezes at the first refused write; the buffer is zeroed by BitStream_Init; not for the
     --acn-v2 types that write a value again at a saved position). Fz / Fb / Fw / Fd pin the freeze of the byte
     writers and of the alignments: a field that would fit after the first refused write is not written. Covers the uPER and ACN integer writers, the leading zeros (a move without a write),
     an octet string write, an align-to-next word and dword, CONTAINING contents in a temporary stream (uPER, legacy ACN) or in
     place with the region length written again from positions (--acn-v2), and a --acn-v2 deferred size determinant
     patched after the contents were refused;
   - pinned rows: positions worked out by hand, including the two silent results of the unchecked writes (a 32-bit
     zero into 2 bytes returned TRUE at byte 2; a value above the range of INTEGER (0 .. 3), encoded without its
     constraint check, moved the position 39 bits back and wrote before the buffer) and the variable-size octet
     string that returned FALSE with error code 0;
   - ASN1SCC_STREAMING: the same rows with no push callback; with a push callback, chunk buffers of 1 .. 8 bytes:
     TRUE, every chunk inside its guards, the pushed bytes equal to the encoding (an align-to-next word is relative
     to the chunk, so with a chunk size that is not a multiple of the alignment only the result and the guards
     are checked);
   - uPER CONTAINING contents above their computed maximum (runtime called directly): FALSE, ERR_BITSTREAM_OVERFLOW;
   - ASN1SCC_UNCHECKED_WRITES (the opt-out): the full-buffer rows only.
   A determinant shared by several consumers can report its own error code after a refused write: acn-wire/023. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "022.h"

#define GUARD 8

/* -DWIRE_ACNV2 (runWireTests.sh, --acn-v2 output): the CONTAINING regions and the deferred determinant are written
   again at saved positions after their contents, so a short buffer holds a placeholder there */
#if defined(WIRE_ACNV2)
#define IN_ORDER_V2 0
#else
#define IN_ORDER_V2 1
#endif
#define GUARD_BYTE 0xA5

typedef flag (*enc_t)(const void* pVal, BitStream* pBitStrm, int* pErrCode, flag bCheckConstraints);

static int failures = 0;

typedef struct {
    flag ok;
    int err;
    long pos;
    int guardsOk;
    byte data[64];
} Outcome;

/* Encodes v into a buffer of k bytes starting at bit startBit, between two guard regions. */
static Outcome encode_into(enc_t f, const void* v, long k, int startBit, flag bCheck)
{
    Outcome o;
    byte* base = (byte*)malloc((size_t)(k + 2 * GUARD));
    BitStream bs;
    long i;
    memset(&o, 0, sizeof(o));
    if (base == NULL) {
        printf("out of memory\n");
        exit(2);
    }
    memset(base, GUARD_BYTE, (size_t)(k + 2 * GUARD));
    BitStream_Init(&bs, base + GUARD, k);
    bs.currentBit = startBit;
    o.err = 0;
    o.ok = f(v, &bs, &o.err, bCheck);
    o.pos = bs.currentByte * 8L + bs.currentBit;
    o.guardsOk = 1;
    for (i = 0; i < GUARD; i++) {
        if (base[i] != GUARD_BYTE || base[GUARD + k + i] != GUARD_BYTE)
            o.guardsOk = 0;
    }
    memcpy(o.data, base + GUARD, (size_t)(k < 64 ? k : 64));
    free(base);
    return o;
}

static void report(const char* name, const char* row, long k, int s, const Outcome* o)
{
    printf("%s %s k=%ld s=%d: result %d, error code %d, position %ld, guards %s\n", name, row, k, s, (int)o->ok, o->err,
           o->pos, o->guardsOk ? "intact" : "OVERWRITTEN");
    failures++;
}

/* The full encoding (expected bits and bytes), then every shorter buffer from start bits 0 .. 7. */
#if defined(ASN1SCC_CHECKED_WRITES)
static const byte zeros[64] = { 0 };

/* Bits [from, to) of a and b are equal */
static int same_bits(const byte* a, const byte* b, long from, long to)
{
    long i;
    for (i = from; i < to; i++) {
        int m = 0x80 >> (i % 8);
        if ((a[i / 8] & m) != (b[i / 8] & m))
            return 0;
    }
    return 1;
}
#endif

/* bInOrder: the encoder writes every bit once, in stream order (no value written again at a saved position), so the
   bits a short buffer holds before the frozen position are those of the full encoding */
static void check_type(const char* name, enc_t f, const void* v, long nBits, const byte* expected, int bInOrder)
{
    long n = (nBits + 7) / 8;
    long k;
    int s;
    Outcome o = encode_into(f, v, n, 0, TRUE);
    if (!o.ok || o.err != 0 || o.pos != nBits || !o.guardsOk || memcmp(o.data, expected, (size_t)n) != 0) {
        long i;
        report(name, "full", n, 0, &o);
        printf("   bytes:");
        for (i = 0; i < n; i++)
            printf(" %02X", o.data[i]);
        printf("\n");
    }
#if defined(ASN1SCC_CHECKED_WRITES)
    for (s = 0; s < 8; s++) {
        /* bytes the encoding needs from bit s (an alignment can absorb the offset) */
        Outcome full = encode_into(f, v, n + 8, s, TRUE);
        long nFrom = (full.pos + 7) / 8;
        if (!full.ok || full.err != 0 || !full.guardsOk)
            report(name, "full from bit s", n + 8, s, &full);
        for (k = 0; k < nFrom; k++) {
            if (k == 0 && s != 0)
                continue;   /* no bit s in an empty buffer */
            o = encode_into(f, v, k, s, TRUE);
            if (o.ok || o.err != ERR_BITSTREAM_OVERFLOW || o.pos > k * 8 || o.pos < s || !o.guardsOk)
                report(name, "short", k, s, &o);
            else if (bInOrder && o.pos <= 64 * 8 && !same_bits(o.data, full.data, s, o.pos))
                report(name, "short, not a prefix of the encoding", k, s, &o);
            else if (bInOrder && k <= 64 && !same_bits(o.data, zeros, o.pos, k * 8))
                report(name, "short, written after the frozen position", k, s, &o);
        }
        /* exactly the bytes the encoding needs from bit s: TRUE, the same end and bytes as in the larger buffer */
        o = encode_into(f, v, nFrom, s, TRUE);
        if (!o.ok || o.err != 0 || o.pos != full.pos || !o.guardsOk ||
            memcmp(o.data, full.data, (size_t)(nFrom < 64 ? nFrom : 64)) != 0)
            report(name, "exact fit", nFrom, s, &o);
    }
#else
    (void)k;
    (void)s;
    (void)bInOrder;
#endif
}

#if defined(ASN1SCC_CHECKED_WRITES)
static void check_row(const char* name, enc_t f, const void* v, long k, flag bCheck, flag expected, int expectedErr,
                      long expectedPos)
{
    Outcome o = encode_into(f, v, k, 0, bCheck);
    if (o.ok != expected || o.err != expectedErr || o.pos != expectedPos || !o.guardsOk)
        report(name, "pinned", k, 0, &o);
}
#endif

#if defined(ASN1SCC_CHECKED_WRITES)
/* uPER OCTET / BIT STRING (CONTAINING X) whose contents are longer than the maximum the compiler computed for them
   (a compiler defect; called directly): the runtime refuses them and sets the overflow flag, and the encoder result
   is FALSE with ERR_BITSTREAM_OVERFLOW (before: FALSE with error code 0). */
static void check_containing_maximum(void)
{
    static byte contents[4] = { 1, 2, 3, 4 };
    byte buf[16];
    BitStream bs;
    int err;
    flag ok;
    BitStream_Init(&bs, buf, sizeof(buf));
    ok = BitStream_EncodeContainingOctets(&bs, contents, 32, 3);
    err = 0;
    ok = BitStream_EncodeResult(&bs, ok, &err);
    if (ok || err != ERR_BITSTREAM_OVERFLOW || !bs.overflow || bs.currentByte != 0 || bs.currentBit != 0) {
        printf("EncodeContainingOctets above the maximum: result %d, error code %d, overflow %d\n", (int)ok, err,
               (int)bs.overflow);
        failures++;
    }
    BitStream_Init(&bs, buf, sizeof(buf));
    ok = BitStream_EncodeContainingBits(&bs, contents, 32, 24);
    err = 0;
    ok = BitStream_EncodeResult(&bs, ok, &err);
    if (ok || err != ERR_BITSTREAM_OVERFLOW || !bs.overflow || bs.currentByte != 0 || bs.currentBit != 0) {
        printf("EncodeContainingBits above the maximum: result %d, error code %d, overflow %d\n", (int)ok, err,
               (int)bs.overflow);
        failures++;
    }
}
#endif

#if defined(ASN1SCC_CHECKED_WRITES)
/* BitStream_AppendByteArray is called by the generated code only through BitStream_EncodeOctetString_no_length,
   which tests the freeze first: called directly on a frozen stream it must write nothing either. */
static void check_frozen_byte_array(void)
{
    static const byte data[2] = { 0x5A, 0xA5 };
    byte buf[4] = { 0, 0, 0, 0 };
    BitStream bs;
    flag ok;
    int s;
    for (s = 0; s < 8; s++) {
        BitStream_Init(&bs, buf, sizeof(buf));
        bs.currentBit = s;
        BitStream_AppendNBitZero(&bs, 40);   /* does not fit: refused, the stream freezes at bit s */
        ok = BitStream_AppendByteArray(&bs, data, 2);
        if (ok || !bs.overflow || bs.currentByte != 0 || bs.currentBit != s || buf[0] != 0 || buf[1] != 0 || buf[2] != 0) {
            printf("AppendByteArray on a frozen stream from bit %d: result %d, position %ld.%d\n", s, (int)ok,
                   bs.currentByte, bs.currentBit);
            failures++;
        }
    }
}
#endif

#if defined(ASN1SCC_STREAMING)
/* The push callback copies each full chunk to pushedBytes. */
static byte pushedBytes[256];
static long nPushed = 0;
void pushData(BitStream* pBitStrm, void* param)
{
    (void)param;
    if (nPushed + pBitStrm->count <= (long)sizeof(pushedBytes))
        memcpy(pushedBytes + nPushed, pBitStrm->buf, (size_t)pBitStrm->count);
    nPushed += pBitStrm->count;
}
void fetchData(BitStream* pBitStrm, void* param)
{
    (void)pBitStrm;
    (void)param;
}

static void check_chunks(const char* name, enc_t f, const void* v, long nBits, const byte* expected, long nAlignment)
{
    long n = (nBits + 7) / 8;
    long c;
    for (c = 1; c <= 8; c++) {
        byte* base = (byte*)malloc((size_t)(c + 2 * GUARD));
        BitStream bs;
        int err = 0;
        int guardsOk = 1;
        flag ok;
        long i;
        long total;
        flag bCompare = c % nAlignment == 0;
        if (base == NULL)
            exit(2);
        memset(base, GUARD_BYTE, (size_t)(c + 2 * GUARD));
        BitStream_Init(&bs, base + GUARD, c);
        bs.pushDataPrm = &nPushed;
        nPushed = 0;
        ok = f(v, &bs, &err, TRUE);
        for (i = 0; i < GUARD; i++) {
            if (base[i] != GUARD_BYTE || base[GUARD + c + i] != GUARD_BYTE)
                guardsOk = 0;
        }
        total = nPushed + bs.currentByte + (bs.currentBit != 0);
        if (bs.currentByte >= 0 && bs.currentByte < c && nPushed + c <= (long)sizeof(pushedBytes))
            memcpy(pushedBytes + nPushed, base + GUARD, (size_t)(bs.currentByte + (bs.currentBit != 0)));
        if (nBits % 8 != 0 && n <= (long)sizeof(pushedBytes))
            pushedBytes[n - 1] &= (byte)(0xFF << (8 - nBits % 8));   /* the bits after the encoding are not written */
        if (!ok || err != 0 || !guardsOk || bs.currentByte < 0 || bs.currentByte >= c ||
            (bCompare && (total != n || memcmp(pushedBytes, expected, (size_t)n) != 0))) {
            printf("%s chunk=%ld: result %d, error code %d, pushed %ld, position %ld.%d, guards %s\n", name, c, (int)ok, err,
                   nPushed, bs.currentByte, bs.currentBit, guardsOk ? "intact" : "OVERWRITTEN");
            failures++;
        }
        free(base);
    }
}
#endif

int main(void)
{
    static U32 u32;
    static Pair pair;
    static Q q;
    static Rec rec;
    static Al al;
    static Ad ad;
    static Box box;
    static Box2 box2;
    static Bits bits;
    static Outer outer;
    static Fz fz;
    static Fb fb;
    static Fw fw;
    static Fd fd;

    static const byte u32Bytes[] = { 0x00, 0x00, 0x00, 0x00 };
    static const byte pairBytes[] = { 0x00, 0x00, 0x00, 0x00, 0xA5 };
    static const byte qBytes[] = { 0x80 };
    static const byte recBytes[] = { 0x11, 0x30, 0x10, 0x20, 0x30, 0x40 };
    static const byte alUperBytes[] = { 0x01, 0x02, 0x03 };
    static const byte alAcnBytes[] = { 0x01, 0x00, 0x02, 0x03 };
    static const byte adAcnBytes[] = { 0x01, 0x00, 0x00, 0x00, 0x02, 0x03 };
    static const byte boxUperBytes[] = { 0x22, 0x06, 0x11, 0x30, 0x10, 0x20, 0x30, 0x40, 0x33 };
    static const byte boxAcnBytes[] = { 0x22, 0x31, 0x13, 0x01, 0x02, 0x03, 0x04, 0x03, 0x30 };
    static const byte box2AcnBytes[] = { 0x22, 0x06, 0x11, 0x30, 0x10, 0x20, 0x30, 0x40, 0x33 };
    static const byte bitsUperBytes[] = { 0x30, 0x11, 0x30, 0x10, 0x20, 0x30, 0x40, 0x33 };
    static const byte bitsAcnBytes[] = { 0x30, 0x22, 0x60, 0x20, 0x40, 0x60, 0x86, 0x60 };
    static const byte outerUperBytes[] = { 0x44, 0x28, 0x08, 0x10, 0x18, 0x20, 0x28 };
    static const byte outerAcnBytes[] = { 0x00, 0x05, 0x44, 0x01, 0x02, 0x03, 0x04, 0x05 };
    static const byte fzBytes[] = { 0x00, 0x00, 0x00, 0x00, 0x5A, 0x11, 0x22 };
    static const byte fbUperBytes[] = { 0xA0, 0x00, 0x00, 0x00, 0x06, 0x60 };
    static const byte fbAcnBytes[] = { 0xA0, 0x00, 0x00, 0x00, 0x00, 0x33 };
    static const byte fwUperBytes[] = { 0x44, 0x00, 0x00, 0x00, 0x00, 0x33 };
    static const byte fwAcnBytes[] = { 0x44, 0x00, 0x00, 0x00, 0x00, 0x00, 0x33 };
    static const byte fdAcnBytes[] = { 0x44, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x33 };

    if (ERR_BITSTREAM_OVERFLOW != -1) {
        printf("ERR_BITSTREAM_OVERFLOW is %d\n", ERR_BITSTREAM_OVERFLOW);
        failures++;
    }

    u32 = 0;
    pair.x = 0;
    pair.y = 0xA5;
    q = 2;
    rec.a = 0x11;
    rec.b.nCount = 4;
    rec.b.arr[0] = 1; rec.b.arr[1] = 2; rec.b.arr[2] = 3; rec.b.arr[3] = 4;
    al.a = 1; al.b = 2; al.c = 3;
    ad.a = 1; ad.b = 2; ad.c = 3;
    box.h = 0x22; box.p = rec; box.t = 0x33;
    box2.h = 0x22; box2.p = rec; box2.t = 0x33;
    bits.p = rec; bits.t = 0x33;
    outer.tag = 0x44;
    outer.payload.data.nCount = 5;
    outer.payload.data.arr[0] = 1; outer.payload.data.arr[1] = 2; outer.payload.data.arr[2] = 3;
    outer.payload.data.arr[3] = 4; outer.payload.data.arr[4] = 5;
    fz.x = 0; fz.y = 0x5A; fz.z.arr[0] = 0x11; fz.z.arr[1] = 0x22;
    fb.a = 5; fb.x = 0; fb.b = 0x33;
    fw.a = 0x44; fw.x = 0; fw.b = 0x33;
    fd.a = 0x44; fd.x = 0; fd.b = 0x33;

    check_type("U32 uPER", (enc_t)U32_Encode, &u32, 32, u32Bytes, 1);
    check_type("U32 ACN", (enc_t)U32_ACN_Encode, &u32, 32, u32Bytes, 1);
    check_type("Pair uPER", (enc_t)Pair_Encode, &pair, 40, pairBytes, 1);
    check_type("Pair ACN", (enc_t)Pair_ACN_Encode, &pair, 40, pairBytes, 1);
    check_type("Q uPER", (enc_t)Q_Encode, &q, 2, qBytes, 1);
    check_type("Q ACN", (enc_t)Q_ACN_Encode, &q, 2, qBytes, 1);
    check_type("Rec uPER", (enc_t)Rec_Encode, &rec, 44, recBytes, 1);
    check_type("Rec ACN", (enc_t)Rec_ACN_Encode, &rec, 44, recBytes, 1);
    check_type("Al uPER", (enc_t)Al_Encode, &al, 24, alUperBytes, 1);
    check_type("Al ACN", (enc_t)Al_ACN_Encode, &al, 32, alAcnBytes, 1);
    check_type("Ad uPER", (enc_t)Ad_Encode, &ad, 24, alUperBytes, 1);
    check_type("Ad ACN", (enc_t)Ad_ACN_Encode, &ad, 48, adAcnBytes, 1);
    check_type("Box uPER", (enc_t)Box_Encode, &box, 72, boxUperBytes, 1);
    check_type("Box ACN", (enc_t)Box_ACN_Encode, &box, 68, boxAcnBytes, IN_ORDER_V2);
    check_type("Box2 uPER", (enc_t)Box2_Encode, &box2, 72, boxUperBytes, 1);
    check_type("Box2 ACN", (enc_t)Box2_ACN_Encode, &box2, 72, box2AcnBytes, IN_ORDER_V2);
    check_type("Bits uPER", (enc_t)Bits_Encode, &bits, 64, bitsUperBytes, 1);
    check_type("Bits ACN", (enc_t)Bits_ACN_Encode, &bits, 59, bitsAcnBytes, IN_ORDER_V2);
    check_type("Outer uPER", (enc_t)Outer_Encode, &outer, 53, outerUperBytes, 1);
    check_type("Outer ACN", (enc_t)Outer_ACN_Encode, &outer, 64, outerAcnBytes, IN_ORDER_V2);
    check_type("Fz uPER", (enc_t)Fz_Encode, &fz, 56, fzBytes, 1);
    check_type("Fz ACN", (enc_t)Fz_ACN_Encode, &fz, 56, fzBytes, 1);
    check_type("Fb uPER", (enc_t)Fb_Encode, &fb, 43, fbUperBytes, 1);
    check_type("Fb ACN", (enc_t)Fb_ACN_Encode, &fb, 48, fbAcnBytes, 1);
    check_type("Fw uPER", (enc_t)Fw_Encode, &fw, 48, fwUperBytes, 1);
    check_type("Fw ACN", (enc_t)Fw_ACN_Encode, &fw, 56, fwAcnBytes, 1);
    check_type("Fd uPER", (enc_t)Fd_Encode, &fd, 48, fwUperBytes, 1);
    check_type("Fd ACN", (enc_t)Fd_ACN_Encode, &fd, 72, fdAcnBytes, 1);

#if defined(ASN1SCC_CHECKED_WRITES)
    /* U32 = 0 is 32 leading zero bits (a move without a write): in 2 bytes the unchecked move ended at byte 2 and the
       encoder returned TRUE */
    check_row("U32 uPER", (enc_t)U32_Encode, &u32, 2, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 0);
    check_row("U32 ACN", (enc_t)U32_ACN_Encode, &u32, 3, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 0);
    /* the zeros of x do not fit: the stream is frozen at bit 0 and y, which would fit, is not written (the unchecked
       zeros moved past the buffer and y was written there); with 4 bytes x fits and y does not */
    check_row("Pair uPER", (enc_t)Pair_Encode, &pair, 2, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 0);
    check_row("Pair ACN", (enc_t)Pair_ACN_Encode, &pair, 4, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 32);
    /* a (8 bits) and the length (4 bits) fit, the 4 octets do not: FALSE with error code 0 before. A streaming build
       writes an octet array byte by byte: the first octet fits there */
#if defined(ASN1SCC_STREAMING)
    check_row("Rec uPER", (enc_t)Rec_Encode, &rec, 3, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 20);
    check_row("Rec ACN", (enc_t)Rec_ACN_Encode, &rec, 3, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 20);
#else
    check_row("Rec uPER", (enc_t)Rec_Encode, &rec, 3, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 12);
    check_row("Rec ACN", (enc_t)Rec_ACN_Encode, &rec, 3, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 12);
#endif
    /* align-to-next word after a: no room for the padding (1 byte), room for it but not for b (2), not for c (3) */
    check_row("Al ACN", (enc_t)Al_ACN_Encode, &al, 1, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 8);
    check_row("Al ACN", (enc_t)Al_ACN_Encode, &al, 2, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 16);
    check_row("Al ACN", (enc_t)Al_ACN_Encode, &al, 3, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 24);
    /* align-to-next dword: in 3 bytes the padding (3 bytes) is refused and the stream stays after a (b and c would
       fit there, they are not written); in 4 bytes the padding fits and b does not */
    check_row("Ad ACN", (enc_t)Ad_ACN_Encode, &ad, 3, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 8);
    check_row("Ad ACN", (enc_t)Ad_ACN_Encode, &ad, 4, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 32);
    /* The freeze of the writers and moves that do not go through the size macros: the 32 leading zero bits of x are
       refused and freeze the stream; what follows would fit and must not be written / moved: y [size 8, encoding
       pos-int] (BitStream_AppendByte0) and z, a byte-aligned OCTET STRING (BitStream_EncodeOctetString_no_length),
       in 3 bytes; the padding of b [align-to-next byte / word / dword] (Acn_AlignToNextByte / Word / DWord) */
    check_row("Fz uPER", (enc_t)Fz_Encode, &fz, 3, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 0);
    check_row("Fz ACN", (enc_t)Fz_ACN_Encode, &fz, 3, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 0);
    check_row("Fb ACN", (enc_t)Fb_ACN_Encode, &fb, 4, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 3);
    check_row("Fw ACN", (enc_t)Fw_ACN_Encode, &fw, 3, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 8);
    check_row("Fd ACN", (enc_t)Fd_ACN_Encode, &fd, 4, TRUE, FALSE, ERR_BITSTREAM_OVERFLOW, 8);
    check_containing_maximum();
    check_frozen_byte_array();
#if WORD_SIZE == 8 && defined(NDEBUG)
    /* 2^40 in INTEGER (0 .. 3): the constraint check rejects it with the type's own error code; encoded without the
       check (bCheckConstraints FALSE), the 2-bit field gets -39 leading zeros: before, the position moved 39 bits
       back and the 41 value bits were written before the buffer (TRUE, error code 0; TRUE at bit 2 in a 1-byte buffer).
       Now the move is refused and the stream is frozen at bit 0: nothing is written (FALSE). Not with assertions on:
       the range assertion of the integer encoder stops the program, as before. */
    q = (Q)1 << 40;
    check_row("Q uPER checked", (enc_t)Q_Encode, &q, 8, TRUE, FALSE, ERR_Q, 0);
    check_row("Q uPER unchecked", (enc_t)Q_Encode, &q, 8, FALSE, FALSE, ERR_BITSTREAM_OVERFLOW, 0);
    check_row("Q ACN unchecked", (enc_t)Q_ACN_Encode, &q, 8, FALSE, FALSE, ERR_BITSTREAM_OVERFLOW, 0);
    check_row("Q uPER unchecked", (enc_t)Q_Encode, &q, 1, FALSE, FALSE, ERR_BITSTREAM_OVERFLOW, 0);
    q = 2;
#endif
#endif

#if defined(ASN1SCC_STREAMING)
    check_chunks("U32 uPER push", (enc_t)U32_Encode, &u32, 32, u32Bytes, 1);
    check_chunks("Pair uPER push", (enc_t)Pair_Encode, &pair, 40, pairBytes, 1);
    check_chunks("Rec uPER push", (enc_t)Rec_Encode, &rec, 44, recBytes, 1);
    check_chunks("Al ACN push", (enc_t)Al_ACN_Encode, &al, 32, alAcnBytes, 2);
    check_chunks("Ad ACN push", (enc_t)Ad_ACN_Encode, &ad, 48, adAcnBytes, 4);
#endif

    if (failures != 0) {
        printf("022: %d failures\n", failures);
        return 1;
    }
    printf("022 OK\n");
    return 0;
}
