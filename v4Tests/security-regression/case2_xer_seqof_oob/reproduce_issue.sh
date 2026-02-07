#!/bin/bash

# Exit on error
set -e

echo "1. Generating code with asn1scc..."
asn1scc.exe -XER -c -o c_out/ -atc a.asn

echo "2. Switching to output directory..."
cd c_out

echo "3. Creating malicious XML input (10 elements, schema allows max 5)..."
cat > test_oob.xml <<EOF
<TestSeq>
    <INTEGER>1</INTEGER>
    <INTEGER>2</INTEGER>
    <INTEGER>3</INTEGER>
    <INTEGER>4</INTEGER>
    <INTEGER>5</INTEGER>
    <INTEGER>6</INTEGER>
    <INTEGER>7</INTEGER>
    <INTEGER>8</INTEGER>
    <INTEGER>9</INTEGER>
    <INTEGER>10</INTEGER>
</TestSeq>
EOF

echo "4. Creating test runner (test_oob.c)..."
cat > test_oob.c <<EOF
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "asn1crt.h"
#include "asn1crt_encoding.h"
#include "a.h"

int main() {
    printf("Reading malicious XML...\n");
    FILE *f = fopen("test_oob.xml", "rb");
    if (!f) {
        perror("fopen");
        return 1;
    }
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    byte *buffer = malloc(fsize);
    if (!buffer) {
        perror("malloc");
        fclose(f);
        return 1;
    }
    fread(buffer, 1, fsize, f);
    fclose(f);

    ByteStream bs;
    ByteStream_Init(&bs, buffer, fsize);
    
    TestSeq val;
    int errCode = 0;
    flag ret;

    // Initialize val to clean state
    TestSeq_Initialize(&val);

    printf("Decoding...\n");
    ret = TestSeq_XER_Decode(&val, &bs, &errCode);

    if (ret == FALSE) {
        printf("PASS: Decode returned FALSE as expected.\n");
        printf("ErrorCode: %d\n", errCode);
        return 0;
    } else {
        printf("FAIL: Decode returned TRUE (Vulnerable to OOB).\n");
        printf("Decoded count: %d\n", val.nCount);
        
        if (val.nCount > 5) {
             printf("CRITICAL: nCount %d exceeds limit 5!\n", val.nCount);
        }
        return 1;
    }
}
EOF

echo "5. Compiling test runner..."
gcc -g -o test_oob.exe test_oob.c a.c asn1crt.c asn1crt_encoding.c asn1crt_encoding_xer.c -I.

echo "6. Running test..."
./test_oob.exe
