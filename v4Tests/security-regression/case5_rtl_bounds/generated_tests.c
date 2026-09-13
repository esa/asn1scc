#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "a.h"
#define CHECK(c) do { if (!(c)) { fprintf(stderr, "FAIL line %d: %s\n", __LINE__, #c); return 1; } } while (0)

int main(int argc, char** argv) {
    const char* test = argc > 1 ? argv[1] : "";
    int err = 0;
    ByteStream xs;
    BitStream bs;
    if (!strcmp(test, "xer-bits")) {
        Bits value;
        byte good[] = "<Bits>10101010</Bits>";
        byte tooLong[] = "<Bits>1111111111111111</Bits>";
        Bits_Initialize(&value);
        ByteStream_AttachBuffer(&xs, good, sizeof(good)-1);
        CHECK(Bits_XER_Decode(&value, &xs, &err) && value.arr[0] == 0xAA);
        ByteStream_AttachBuffer(&xs, tooLong, sizeof(tooLong)-1);
        CHECK(!Bits_XER_Decode(&value, &xs, &err) && err != 0);
    } else if (!strcmp(test, "xer-scalars")) {
        Signed value;
        Identifier oid;
        byte invalid[] = "<Signed>12junk</Signed>";
        ByteStream_AttachBuffer(&xs, invalid, sizeof(invalid)-1);
        CHECK(!Signed_XER_Decode(&value, &xs, &err));
        byte good[] = "<Identifier>1.2.840.113549</Identifier>";
        Identifier_Initialize(&oid);
        ByteStream_AttachBuffer(&xs, good, sizeof(good)-1);
        CHECK(Identifier_XER_Decode(&oid, &xs, &err) && oid.nCount == 4 && oid.values[2] == 840);
        Text str;
        byte goodText[] = "<Text>abc</Text>";
        ByteStream_AttachBuffer(&xs, goodText, sizeof(goodText)-1);
        CHECK(Text_XER_Decode(str, &xs, &err) && !strcmp(str, "abc"));
        byte longText[] = "<Text>abcdef</Text>";
        ByteStream_AttachBuffer(&xs, longText, sizeof(longText)-1);
        CHECK(!Text_XER_Decode(str, &xs, &err));
        Label label;
        byte known[] = "<Label><first/></Label>";
        ByteStream_AttachBuffer(&xs, known, sizeof(known)-1);
        CHECK(Label_XER_Decode(&label, &xs, &err));
        byte unknown[] = "<Label><third/></Label>";
        ByteStream_AttachBuffer(&xs, unknown, sizeof(unknown)-1);
        CHECK(!Label_XER_Decode(&label, &xs, &err));
        Pair pair;
        byte fullPair[] = "<Pair><Pair-elem>1</Pair-elem><Pair-elem>2</Pair-elem></Pair>";
        ByteStream_AttachBuffer(&xs, fullPair, sizeof(fullPair)-1);
        CHECK(Pair_XER_Decode(&pair, &xs, &err) && pair.arr[0] == 1 && pair.arr[1] == 2);
        byte shortPair[] = "<Pair><Pair-elem>1</Pair-elem></Pair>";
        ByteStream_AttachBuffer(&xs, shortPair, sizeof(shortPair)-1);
        CHECK(!Pair_XER_Decode(&pair, &xs, &err));
        Select selected;
        byte goodChoice[] = "<Select><selection><first/></selection></Select>";
        ByteStream_AttachBuffer(&xs, goodChoice, sizeof(goodChoice)-1);
        CHECK(Select_XER_Decode(&selected, &xs, &err));
        byte badChoice[] = "<Select><selection><third/></selection></Select>";
        ByteStream_AttachBuffer(&xs, badChoice, sizeof(badChoice)-1);
        CHECK(!Select_XER_Decode(&selected, &xs, &err));
        Small small;
        byte goodSmall[] = "<Small>7</Small>";
        ByteStream_AttachBuffer(&xs, goodSmall, sizeof(goodSmall)-1);
        CHECK(Small_XER_Decode(&small, &xs, &err) && small == 7);
        byte bigSmall[] = "<Small>256</Small>";
        ByteStream_AttachBuffer(&xs, bigSmall, sizeof(bigSmall)-1);
        CHECK(!Small_XER_Decode(&small, &xs, &err));
    } else if (!strcmp(test, "binary-bounds")) {
        Truth truth;
        Small small;
        Signed signedValue;
        byte data[1] = {0xE0};
        BitStream_AttachBuffer(&bs, data, 1);
        CHECK(Truth_Decode(&truth, &bs, &err) && truth);
        BitStream_AttachBuffer(&bs, data, 0);
        CHECK(!Truth_Decode(&truth, &bs, &err));
        BitStream_AttachBuffer(&bs, data, 0);
        CHECK(!Small_Decode(&small, &bs, &err));
        data[0] = 1;
        BitStream_AttachBuffer(&bs, data, 1);
        CHECK(!Signed_Decode(&signedValue, &bs, &err));
        BitStream_AttachBuffer(&bs, data, 0);
        CHECK(!Truth_ACN_Decode(&truth, &bs, &err));
    } else if (!strcmp(test, "acn-string")) {
        Text value;
        byte good[] = {'a','b','c',0,0};
        byte longer[] = {'a','b','c','d',0,0};
        BitStream_AttachBuffer(&bs, good, sizeof(good));
        CHECK(Text_ACN_Decode(value, &bs, &err) && !strcmp(value, "abc"));
        BitStream_AttachBuffer(&bs, longer, sizeof(longer));
        CHECK(!Text_ACN_Decode(value, &bs, &err));
    } else if (!strcmp(test, "xer-real")) {
        Number value = INFINITY, decoded = 0;
        byte buf[256];
        ByteStream_Init(&xs, buf, sizeof(buf));
        CHECK(Number_XER_Encode(&value, &xs, &err, TRUE));
        long length = xs.currentByte;
        ByteStream_AttachBuffer(&xs, buf, length);
        CHECK(Number_XER_Decode(&decoded, &xs, &err) && isinf(decoded));
    } else { return 2; }
    printf("PASS generated %s\n", test);
    return 0;
}
