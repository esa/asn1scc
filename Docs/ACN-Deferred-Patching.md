# ACN Deferred Patching (`--acn-v2`)

Deferred patching is an experimental code generation strategy for ACN encoding that eliminates temporary buffers, produces thread-safe code, and generates smaller, more readable C functions.

## The Problem

Consider a typical satellite protocol PDU where a header carries length and size fields that describe data appearing later in the bitstream:

**ASN.1 definition:**

```asn1
MyPDU ::= SEQUENCE {
   header  MyHeader,
   payload OCTET STRING (CONTAINING MyPayload)
}

MyHeader ::= SEQUENCE {}

MyPayload ::= SEQUENCE {
   a       INTEGER (0..1024),
   buffer1 OCTET STRING (SIZE(0..100))
}
```

**ACN annotations:**

```
MyPDU [] {
   header [] {
      payload-length  INTEGER [encoding pos-int, size 16],
      buffer1-length  INTEGER [encoding pos-int, size 8]
   },
   payload [size header.payload-length] {
      a       [encoding pos-int, size 32],
      buffer1 [size header.buffer1-length]
   }
}
```

The header contains two ACN-inserted determinant fields: `payload-length` (16 bits) controls the size of the entire `payload` blob, and `buffer1-length` (8 bits) controls the size of `buffer1` inside the payload. Both values must be written to the bitstream *before* the payload data, but their values are only known *after* encoding the payload.

### Legacy generated code

Without `--acn-v2`, the compiler solves this chicken-and-egg problem by encoding the payload to a temporary buffer first, measuring the result, then writing the determinant values and copying the bytes:

```c
flag MyPDU_ACN_Encode(const MyPDU* pVal, BitStream* pBitStrm,
                      int* pErrCode, flag bCheckConstraints)
{
    flag ret = TRUE;
    asn1SccUint MyPDU_header_payload_length;
    asn1SccUint MyPDU_header_buffer1_length;

    /* Problem 1: static buffer -- not thread-safe, not reentrant */
    static byte arr[MyPayload_REQUIRED_BYTES_FOR_ACN_ENCODING];
    BitStream bitStrm;

    /* Step 1: encode the ENTIRE payload to a temporary bitstream */
    BitStream_Init(&bitStrm, arr, sizeof(arr));
    BitStream* pBitStrm_save = pBitStrm;
    pBitStrm = &bitStrm;

    /* Problem 3: parent function reaches directly into child fields */
    Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm, pVal->payload.a);
    ret = BitStream_EncodeOctetString_no_length(pBitStrm,
              pVal->payload.buffer1.arr, pVal->payload.buffer1.nCount);
    pBitStrm = pBitStrm_save;

    /* Step 2: compute determinant values from the temporary encoding */
    MyPDU_header_payload_length = bitStrm.currentBit == 0
        ? bitStrm.currentByte : (bitStrm.currentByte + 1);
    MyPDU_header_buffer1_length = pVal->payload.buffer1.nCount;

    /* Step 3: write determinant values to the real stream */
    Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16(pBitStrm,
        MyPDU_header_payload_length);
    Acn_Enc_Int_PositiveInteger_ConstSize_8(pBitStrm,
        MyPDU_header_buffer1_length);

    /* Step 4: copy the already-encoded payload bytes -- Problem 2: double encoding */
    ret = BitStream_EncodeOctetString_no_length(pBitStrm,
              arr, (int)MyPDU_header_payload_length);

    return ret;
}
```

This approach has three problems:

1. **Static buffer** (`static byte arr[...]`) -- the temporary buffer is `static`, making the function non-reentrant and not thread-safe. Using a stack buffer instead risks stack overflow for large payloads.

2. **Double encoding** -- the payload data is encoded twice: first to the temporary buffer (to learn its size), then the raw bytes are bulk-copied to the real bitstream.

3. **Monolithic function** -- the parent `MyPDU_ACN_Encode` directly accesses child type fields (`pVal->payload.a`, `pVal->payload.buffer1.arr`). No calls to `MyPayload_ACN_Encode` are generated. Everything is inlined into one large function that is hard to read and debug.


## The Solution: Deferred Patching

Instead of computing determinant values upfront, deferred patching uses a three-step approach:

1. **Reserve space** -- write placeholder bits at the determinant's position and save that position
2. **Encode data** -- encode the payload fields directly to the real bitstream (single pass)
3. **Patch** -- seek back to the saved position, write the now-known determinant value, and restore the stream position

### Runtime primitives

The C runtime provides three building blocks:

```c
/* Holds a saved bitstream position + the determinant value */
typedef struct {
    AcnBitStreamPos pos;     /* where in the stream the determinant lives */
    flag            is_set;  /* has the value been written? (for shared determinants) */
    asn1SccUint     value;   /* the determinant value */
} AcnInsertedFieldRef;

/* Reserve space: write 'size' zero bits, save position in 'det' */
void Acn_InitDet_<name>(BitStream* pBitStrm, AcnInsertedFieldRef* det);

/* Patch: seek back to det->pos, write value 'v', restore stream position */
flag Acn_PatchDet_<name>(asn1SccUint v, BitStream* pBitStrm,
                         AcnInsertedFieldRef* det, int* pErrCode);
```

The `<name>` suffix matches the encoding class -- for example, `Acn_InitDet_U16_BE` / `Acn_PatchDet_U16_BE` for a 16-bit big-endian unsigned integer, or `Acn_InitDet_U8` / `Acn_PatchDet_U8` for an 8-bit unsigned.

### Generated code with `--acn-v2`

With deferred patching, the compiler produces three focused functions instead of one monolithic block:

**Parent function** -- a thin orchestrator:

```c
flag MyPDU_ACN_Encode(const MyPDU* pVal, BitStream* pBitStrm,
                      int* pErrCode, flag bCheckConstraints)
{
    flag ret = TRUE;
    AcnInsertedFieldRef buffer1_length;
    AcnInsertedFieldRef payload_length;

    /*Encode header*/
    ret = MyPDU_header_ACN_Encode(&pVal->header, pBitStrm, pErrCode, FALSE,
                                  &buffer1_length, &payload_length);
    if (ret) {
        /*Encode payload*/
        ret = MyPDU_payload_ACN_Encode(&pVal->payload, pBitStrm, pErrCode, FALSE,
                                       &buffer1_length, &payload_length);
    }
    return ret;
}
```

The parent declares `AcnInsertedFieldRef` structs on the stack and passes them by pointer to the child functions. No temporary buffers, no direct access to child type fields.

**Header encoder** -- reserves space for determinants:

```c
flag MyPDU_header_ACN_Encode(const MyHeader* pVal, BitStream* pBitStrm,
    int* pErrCode, flag bCheckConstraints,
    AcnInsertedFieldRef* MyPDU_header_buffer1_length,
    AcnInsertedFieldRef* MyPDU_header_payload_length)
{
    flag ret = TRUE;

    /* Reserve 16 bits for payload-length, save position */
    Acn_InitDet_U16_BE(pBitStrm, MyPDU_header_payload_length);
    if (ret) {
        /* Reserve 8 bits for buffer1-length, save position */
        Acn_InitDet_U8(pBitStrm, MyPDU_header_buffer1_length);
    }
    return ret;
}
```

**Payload encoder** -- encodes data, then patches determinant values:

```c
flag MyPDU_payload_ACN_Encode(const MyPayload* pVal, BitStream* pBitStrm,
    int* pErrCode, flag bCheckConstraints,
    AcnInsertedFieldRef* MyPDU_payload_buffer1_length,
    AcnInsertedFieldRef* MyPDU_payload_payload_length)
{
    flag ret = TRUE;

    {
        AcnBitStreamPos acn_data_start = Acn_BitStream_GetPos(pBitStrm);

        /*Encode a*/
        Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32(pBitStrm, pVal->a);
        if (ret) {
            /*Encode buffer1*/
            ret = BitStream_EncodeOctetString_no_length(pBitStrm,
                      pVal->buffer1.arr, pVal->buffer1.nCount);
        }

        if (ret) {
            /* Measure how many bytes were written */
            AcnBitStreamPos acn_data_end = Acn_BitStream_GetPos(pBitStrm);
            asn1SccUint acn_nCount = Acn_BitStream_DistanceInBytes(
                                         acn_data_start, acn_data_end);
            /* Patch payload-length back in the header */
            ret = Acn_PatchDet_U16_BE((asn1SccUint)acn_nCount, pBitStrm,
                                       MyPDU_payload_payload_length, pErrCode);
        }
    }

    /* Patch buffer1-length back in the header */
    ret = Acn_PatchDet_U8((asn1SccUint)pVal->buffer1.nCount, pBitStrm,
                           MyPDU_payload_buffer1_length, pErrCode);

    return ret;
}
```

The payload encoder writes field data directly to the real bitstream in a single pass. After encoding, it measures the byte distance (for `CONTAINING` size) and patches both determinant values back into their reserved positions in the header.

### Shared determinant consistency

When the same determinant is consumed by multiple fields, `Acn_PatchDet_*` performs a consistency check: if `is_set` is already true, it verifies that the new value matches the previously written one. A mismatch returns `ERR_ACN_DET_CONSISTENCY_MISMATCH`.


## How to Use It

Add `--acn-v2` to the `asn1scc` command line:

```bash
asn1scc -c -ACN --acn-v2 -atc -o out/ myfile.asn1 myfile.acn
```

The alternate flag name `-acnDeferred` is also accepted.

**Limitations:**
- C backend only (experimental)
- When `--acn-v2` is omitted, the compiler produces identical output to before -- there is no regression

## Benefits

- **Thread-safe** -- no `static` buffers; all state lives on the stack
- **No stack overflow risk** -- no large temporary byte arrays
- **Single-pass encoding** -- data is written once, directly to the output bitstream
- **Readable code** -- each function encodes only its own type's fields
- **Proper function decomposition** -- the parent is a thin orchestrator; type-specific logic stays in type-specific functions

## Supported ACN Patterns

The following cross-boundary ACN patterns are handled by deferred patching:

| Pattern | Description |
|---------|-------------|
| `CONTAINING` (OCTET STRING) | Size measured in bytes via `Acn_BitStream_DistanceInBytes` |
| `CONTAINING` (BIT STRING) | Size measured in bits via `Acn_BitStream_DistanceInBits` |
| Size determinant | Integer field determines array/string element count |
| Presence (boolean) | 1-bit flag determines OPTIONAL field presence |
| Presence (integer) | Integer value encodes presence condition |
| CHOICE determinant | Enumerated field selects CHOICE alternative |
| Cross-boundary references | Determinant in one type, data in another (passed via `AcnInsertedFieldRef*`) |
| Shared determinants | Same determinant consumed by multiple fields (consistency-checked) |
