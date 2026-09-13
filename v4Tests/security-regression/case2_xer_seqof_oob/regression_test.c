#include <stdio.h>
#include <string.h>
#include "a.h"

int main(int argc, char** argv) {
    char input[512] = "<TestSeq>";
    TestSeq value;
    ByteStream stream;
    int error = 0;
    flag expected = argc == 2 && strcmp(argv[1], "valid") == 0;
    int items = expected ? 5 : 10;
    for (int i = 0; i < items; i++) strcat(input, "<INTEGER>1</INTEGER>");
    strcat(input, "</TestSeq>");
    TestSeq_Initialize(&value);
    ByteStream_AttachBuffer(&stream, (byte*)input, (long)strlen(input));
    flag result = TestSeq_XER_Decode(&value, &stream, &error);
    if (result != expected || (!result && error == 0) || (result && value.nCount != 5)) return 1;
    if (result) for (int i = 0; i < value.nCount; i++) if (value.arr[i] != 1) return 1;
    printf("PASS case2 %s\n", expected ? "valid" : "oversize");
    return 0;
}
