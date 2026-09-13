#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <limits.h>
#include "asn1crt_encoding_acn.h"
#include "asn1crt_encoding_xer.h"
#include "asn1crt_encoding_ber.h"

extern flag Xer_DecodePrimitiveElement(ByteStream*, const char*, char*, size_t, int*);
extern flag ByteStream_AppendString(ByteStream*, const char*);
extern void Xer_EncodeXmlHeader(ByteStream*, const char*);
#define CHECK(c) do { if (!(c)) { fprintf(stderr, "FAIL line %d: %s\n", __LINE__, #c); return 1; } } while (0)

static void xml(ByteStream* s, const char* text) {
    ByteStream_AttachBuffer(s, (byte*)text, (long)strlen(text));
}

int main(int argc, char** argv) {
    BitStream bs;
    ByteStream xs;
    int err = 0, count = 0;
    asn1SccUint u = 0;
    asn1SccSint v = 0;
    asn1Real real = 0;
    byte input[64] = {0}, output[256] = {0};
    const char* test = argc > 1 ? argv[1] : "";
    if (!strcmp(test, "bits")) {
        byte one[1] = {0xA5};
        flag bit;
        BitStream_AttachBuffer(&bs, one, 1);
        for (int i = 0; i < 8; i++) CHECK(BitStream_ReadBit(&bs, &bit));
        CHECK(!BitStream_ReadBit(&bs, &bit));
        CHECK(!BitStream_PeekBit(&bs));
        CHECK(!BitStream_ReadPartialByte(&bs, output, 1));
        BitStream_AttachBuffer(&bs, one, 1);
        bs.currentBit = 7;
        CHECK(!BitStream_ReadPartialByte(&bs, output, 2));
        CHECK(bs.currentByte == 0 && bs.currentBit == 7);
        CHECK(BitStream_ReadPartialByte(&bs, output, 1) && output[0] == 1);
    } else if (!strcmp(test, "byte-arrays")) {
        byte one[1] = {0xA5}, dest[1] = {0};
        BitStream_AttachBuffer(&bs, one, 1);
        CHECK(BitStream_ReadByteArray(&bs, dest, 1) && dest[0] == 0xA5);
        BitStream_Init(&bs, dest, 1);
        CHECK(BitStream_AppendByteArray(&bs, one, 1) && dest[0] == 0xA5);
        CHECK(!BitStream_AppendByte0(&bs, 0));
        BitStream_AttachBuffer(&bs, input, 2);
        input[0] = 0xD2; input[1] = 0x80; bs.currentBit = 1;
        CHECK(BitStream_ReadByteArray(&bs, dest, 1) && dest[0] == 0xA5);
        CHECK(!BitStream_ReadByteArray(&bs, dest, 1));
    } else if (!strcmp(test, "octet-status")) {
        BitStream_AttachBuffer(&bs, input, 0);
        CHECK(!BitStream_DecodeOctetString(&bs, output, &count, 1, 1));
        CHECK(!BitStream_DecodeOctetString_no_length(&bs, output, -1));
        CHECK(!BitStream_EncodeOctetString_no_length(&bs, output, -1));
        BitStream_AttachBuffer(&bs, input, 1);
        CHECK(BitStream_DecodeOctetString(&bs, output, &count, 1, 1) && count == 1);
    } else if (!strcmp(test, "signed")) {
        const asn1SccSint lo = -(asn1SccSint)(MAX_INT >> 1) - 1;
        const asn1SccSint hi = (asn1SccSint)(MAX_INT >> 1);
        asn1SccSint values[] = {lo, -1, 0, hi};
        for (unsigned i = 0; i < sizeof(values)/sizeof(values[0]); i++) {
            BitStream_Init(&bs, input, sizeof(input));
            BitStream_EncodeConstraintWholeNumber(&bs, values[i], lo, hi);
            BitStream_AttachBuffer(&bs, input, WORD_SIZE);
            CHECK(BitStream_DecodeConstraintWholeNumber(&bs, &v, lo, hi) && v == values[i]);
            BitStream_Init(&bs, input, sizeof(input));
            BitStream_EncodeUnConstraintWholeNumber(&bs, values[i]);
            long size = (long)BitStream_GetLength(&bs);
            BitStream_AttachBuffer(&bs, input, size);
            CHECK(BitStream_DecodeUnConstraintWholeNumber(&bs, &v) && v == values[i]);
            BitStream_Init(&bs, input, sizeof(input));
            Acn_Enc_Int_TwosComplement_ConstSize(&bs, values[i], WORD_SIZE * 8);
            BitStream_AttachBuffer(&bs, input, WORD_SIZE);
            CHECK(Acn_Dec_Int_TwosComplement_ConstSize(&bs, &v, WORD_SIZE * 8) && v == values[i]);
        }
        input[0] = 0;
        BitStream_AttachBuffer(&bs, input, 1);
        CHECK(!BitStream_DecodeUnConstraintWholeNumber(&bs, &v));
    } else if (!strcmp(test, "ascii")) {
        const asn1SccSint lo = -(asn1SccSint)(MAX_INT >> 1) - 1;
        const byte zero[] = {0};
        BitStream_Init(&bs, input, sizeof(input));
        Acn_Enc_SInt_ASCII_VarSize_NullTerminated(&bs, lo, zero, 1);
        long size = (long)BitStream_GetLength(&bs);
        BitStream_AttachBuffer(&bs, input, size);
        CHECK(Acn_Dec_SInt_ASCII_VarSize_NullTerminated(&bs, &v, zero, 1) && v == lo);
        BitStream_AttachBuffer(&bs, (byte*)"12x", 3);
        CHECK(!Acn_Dec_UInt_ASCII_ConstSize(&bs, &u, 3));
        BitStream_AttachBuffer(&bs, (byte*)"999999999999999999999", 21);
        CHECK(!Acn_Dec_UInt_ASCII_ConstSize(&bs, &u, 21));
        BitStream_AttachBuffer(&bs, (byte*)"x\0", 2);
        CHECK(!Acn_Dec_UInt_ASCII_VarSize_NullTerminated(&bs, &u, zero, 1));
    } else if (!strcmp(test, "bcd")) {
        input[0] = 0xA0;
        BitStream_AttachBuffer(&bs, input, 1);
        CHECK(!Acn_Dec_Int_BCD_ConstSize(&bs, &u, 1));
        BitStream_AttachBuffer(&bs, input, 1);
        CHECK(!Acn_Dec_Int_BCD_VarSize_NullTerminated(&bs, &u));
        BitStream_Init(&bs, input, sizeof(input));
        Acn_Enc_Int_BCD_VarSize_LengthEmbedded(&bs, MAX_INT);
        long size = (long)BitStream_GetLength(&bs);
        BitStream_AttachBuffer(&bs, input, size);
        CHECK(Acn_Dec_Int_BCD_VarSize_LengthEmbedded(&bs, &u) && u == MAX_INT);
        BitStream_Init(&bs, output, sizeof(output));
        Acn_Enc_UInt_ASCII_ConstSize(&bs, 1, 101);
        CHECK(output[0] == '0' && output[100] == '1');
        BitStream_Init(&bs, output, sizeof(output));
        Acn_Enc_Int_BCD_ConstSize(&bs, 1, 101);
        CHECK(output[0] == 0 && output[50] == 0x10);
    } else if (!strcmp(test, "narrowing")) {
        uint8_t small = 77;
        int8_t signedSmall = 77;
        input[0] = 1; input[1] = 0;
        BitStream_AttachBuffer(&bs, input, 2);
        CHECK(!Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16UInt8(&bs, &small));
        CHECK(small == 77);
        BitStream_AttachBuffer(&bs, input, 0);
        CHECK(!Acn_Dec_Int_PositiveInteger_ConstSize_8UInt8(&bs, &small) && small == 77);
        BitStream_AttachBuffer(&bs, input, 2);
        CHECK(!Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16Int8(&bs, &signedSmall));
    } else if (!strcmp(test, "acn-string")) {
        char str[4];
        const byte term[] = {0, 0};
        BitStream_AttachBuffer(&bs, (byte*)"abc\0\0", 5);
        CHECK(Acn_Dec_String_Ascii_Null_Terminated_mult(&bs, 3, term, 2, str) && !strcmp(str, "abc"));
        BitStream_AttachBuffer(&bs, (byte*)"abcd\0\0", 6);
        CHECK(!Acn_Dec_String_Ascii_Null_Terminated_mult(&bs, 3, term, 2, str));
        CHECK(str[3] == 0);
        CHECK(!Acn_Dec_String_Ascii_Null_Terminated_mult(&bs, 3, term, 0, str));
        CHECK(!Acn_Dec_String_Ascii_External_Field_Determinant(&bs, 3, 4, str));
    } else if (!strcmp(test, "xer-bits")) {
        byte tiny[1] = {0xA5};
        xml(&xs, "<x>10101010</x>");
        CHECK(Xer_DecodeBitString(&xs, "x", tiny, 1, &count, &err) && count == 8 && tiny[0] == 0xAA);
        xml(&xs, "<x>1111111111111111</x>");
        CHECK(!Xer_DecodeBitString(&xs, "x", tiny, 1, &count, &err));
        xml(&xs, "<x>2</x>");
        CHECK(!Xer_DecodeBitString(&xs, "x", tiny, 1, &count, &err));
    } else if (!strcmp(test, "xer-octets")) {
        byte tiny[1];
        xml(&xs, "<x>AA</x>");
        CHECK(Xer_DecodeOctetString(&xs, "x", tiny, 1, &count, &err) && count == 1 && tiny[0] == 0xAA);
        xml(&xs, "<x>AABB</x>");
        CHECK(!Xer_DecodeOctetString(&xs, "x", tiny, 1, &count, &err));
        xml(&xs, "<x>A</x>");
        CHECK(!Xer_DecodeOctetString(&xs, "x", tiny, 1, &count, &err));
    } else if (!strcmp(test, "xer-scalars")) {
        xml(&xs, "<x>12junk</x>"); CHECK(!Xer_DecodeInteger(&xs, "x", &v, &err));
        xml(&xs, "<x>-1</x>"); CHECK(!Xer_DecodePosInteger(&xs, "x", &u, &err));
        xml(&xs, "<x>junk</x>"); CHECK(!Xer_DecodeReal(&xs, "x", &real, &err));
        xml(&xs, "<x>12</x>"); CHECK(Xer_DecodeInteger(&xs, "x", &v, &err) && v == 12);
        xml(&xs, "<x>1</x>");
        CHECK(!Xer_DecodePrimitiveElement(&xs, "x", (char*)output, 0, &err));
        Asn1ObjectIdentifier oid = {0};
        xml(&xs, "<x>1.2.840.113549</x>");
        CHECK(Xer_DecodeObjectIdentifier(&xs, "x", &oid, &err) && oid.nCount == 4 && oid.values[2] == 840);
    } else if (!strcmp(test, "xer-output")) {
        byte nonzero[8]; memset(nonzero, 'X', sizeof(nonzero));
        ByteStream_AttachBuffer(&xs, nonzero, sizeof(nonzero));
        CHECK(ByteStream_AppendString(&xs, "abc") && !strcmp((char*)nonzero, "abc"));
        byte tiny[1] = {0xA5};
        ByteStream_AttachBuffer(&xs, tiny, 1); Xer_EncodeXmlHeader(&xs, NULL);
        CHECK(tiny[0] == 0xA5 && xs.currentByte == 0);
        ByteStream_Init(&xs, output, sizeof(output));
        CHECK(Xer_EncodeReal(&xs, "x", INFINITY, &err, 0));
        ByteStream_AttachBuffer(&xs, output, (long)strlen((char*)output));
        CHECK(Xer_DecodeReal(&xs, "x", &real, &err) && isinf(real));
    } else if (!strcmp(test, "oid")) {
        Asn1ObjectIdentifier oid = {0};
        input[0] = 1; input[1] = 0x80;
        BitStream_AttachBuffer(&bs, input, 2); CHECK(!ObjectIdentifier_uper_decode(&bs, &oid));
        input[0] = 0;
        BitStream_AttachBuffer(&bs, input, 1); CHECK(!ObjectIdentifier_uper_decode(&bs, &oid));
        oid.nCount = 3; oid.values[0] = 2; oid.values[1] = 999; oid.values[2] = 1;
        BitStream_Init(&bs, input, sizeof(input)); ObjectIdentifier_uper_encode(&bs, &oid);
        long size = (long)BitStream_GetLength(&bs);
        BitStream_AttachBuffer(&bs, input, size);
        CHECK(ObjectIdentifier_uper_decode(&bs, &oid) && oid.nCount == 3 && oid.values[0] == 2 && oid.values[1] == 999);
        oid.nCount = OBJECT_IDENTIFIER_MAX_LENGTH + 1; CHECK(!ObjectIdentifier_isValid(&oid));
    } else if (!strcmp(test, "real")) {
        byte data[] = {7, 0x83, 4, 0x7F, 0xFF, 0xFF, 0xFF, 1};
        BitStream_AttachBuffer(&bs, data, sizeof(data));
        CHECK(BitStream_DecodeReal(&bs, &real) && isinf(real));
        BitStream_Init(&bs, input, sizeof(input)); BitStream_EncodeReal(&bs, -1.5);
        long size = (long)BitStream_GetLength(&bs);
        BitStream_AttachBuffer(&bs, input, size);
        CHECK(BitStream_DecodeReal(&bs, &real) && real == -1.5);
    } else if (!strcmp(test, "ber")) {
        byte tag[1] = {2};
        ByteStream_AttachBuffer(&xs, tag, 1); CHECK(!BerDecodeInteger(&xs, 2, &v, &err));
        ByteStream_AttachBuffer(&xs, tag, 0); CHECK(!BerEncodeTag(&xs, 2, &err));
        byte badInt[] = {2, 0};
        ByteStream_AttachBuffer(&xs, badInt, sizeof(badInt)); CHECK(!BerDecodeInteger(&xs, 2, &v, &err));
        byte badBits[] = {3, 1, 8};
        ByteStream_AttachBuffer(&xs, badBits, sizeof(badBits)); CHECK(!BerDecodeBitString(&xs, 3, output, &count, 8, &err));
        ByteStream_Init(&xs, input, sizeof(input)); CHECK(BerEncodeInteger(&xs, 2, -5, &err));
        long size = xs.currentByte;
        ByteStream_AttachBuffer(&xs, input, size); CHECK(BerDecodeInteger(&xs, 2, &v, &err) && v == -5);
    } else { fprintf(stderr, "Unknown test: %s\n", test); return 2; }
    printf("PASS %s\n", test);
    return 0;
}
