#include <stdio.h>
#include <string.h>
#include "a.h"

int main(int argc, char** argv) {
    char input[512];
    PDU value;
    ByteStream stream;
    int error = 0;
    flag expected = argc == 2 && strcmp(argv[1], "valid") == 0;
    if (expected) {
        strcpy(input, "<PDU><a>1</a><b>2</b><c>3</c><d>4</d></PDU>");
    } else {
        strcpy(input, "<PDU><a>");
        memset(input + 8, '1', 300);
        strcpy(input + 308, "</a><b>2</b><c>3</c><d>4</d></PDU>");
    }
    PDU_Initialize(&value);
    ByteStream_AttachBuffer(&stream, (byte*)input, (long)strlen(input));
    flag result = PDU_XER_Decode(&value, &stream, &error);
    if (result != expected || (!result && error == 0) ||
        (result && (value.a != 1 || value.b != 2 || value.c != 3 || value.d != 4))) return 1;
    printf("PASS case1 %s\n", expected ? "valid" : "oversize");
    return 0;
}
