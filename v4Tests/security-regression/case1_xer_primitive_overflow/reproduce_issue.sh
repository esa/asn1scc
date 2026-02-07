#!/bin/bash

# Configuration
ASN1SCC="asn1scc.exe"
OUT_DIR="c_out"
RUNNER="runner.c"
RUNNER_EXE="runner.exe"
XML_FILE="malicious.xml"

# Cleanup previous run
rm -rf $OUT_DIR $RUNNER $RUNNER_EXE $XML_FILE

# 1. Generate Code using asn1scc
echo "Step 1: Generating C code..."
mkdir -p $OUT_DIR
$ASN1SCC -XER -c -o $OUT_DIR -atc a.asn
if [ $? -ne 0 ]; then
    echo "Error: ASN1SCC compilation failed."
    exit 1
fi

# 2. Prepare Input (Malicious XML)
echo "Step 2: Creating malicious input ($XML_FILE)..."
# Create a string of 300 '1's for the integer 'a' to overflow the 256 byte buffer
# We construct the payload manually to ensure portability
PAYLOAD=""
for i in {1..300}; do PAYLOAD="${PAYLOAD}1"; done

# Note: The structure is PDU ::= SEQUENCE { a INTEGER, b INTEGER, c MyInt, d BYTE }
# XML format for XER usually uses the field names as tags.
echo "<PDU><a>$PAYLOAD</a><b>1</b><c>1</c><d>1</d></PDU>" > $XML_FILE

# 3. Create Test Runner dynamically
echo "Step 3: Creating runner source code..."
cat << 'EOF' > $RUNNER
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Include generated headers */
#include "a.h" 

int main() {
    const char *filename = "malicious.xml";
    FILE *f = fopen(filename, "rb");
    if (!f) {
        fprintf(stderr, "Error: Could not open %s\n", filename);
        return 1;
    }

    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *buffer = (char *)malloc(fsize + 1);
    if (!buffer) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        fclose(f);
        return 1;
    }

    size_t read_size = fread(buffer, 1, fsize, f);
    buffer[read_size] = 0;
    fclose(f);

    printf("Read %ld bytes from %s\n", (long)read_size, filename);

    /* Initialize decoding structures */
    PDU pdu;
    PDU_Initialize(&pdu);

    ByteStream strm;
    ByteStream_Init(&strm, (byte*)buffer, read_size);

    int errorCode = 0;
    
    printf("Attempting to decode malicious input...\n");
    flag result = PDU_XER_Decode(&pdu, &strm, &errorCode);

    free(buffer);

    /* Validation Logic */
    /* If the fix works, it should detect the overflow/length and return FALSE */
    if (result == 0) { /* FALSE is 0 */
        printf("PASS: Decode failed as expected. Error Code: %d\n", errorCode);
        return 0;
    } else {
        printf("FAIL: Decode succeeded unexpectedly! Buffer overflow check missing.\n");
        return 1;
    }
}
EOF

# 4. Compile
echo "Step 4: Compiling test runner..."
gcc -o $RUNNER_EXE $RUNNER \
    $OUT_DIR/a.c \
    $OUT_DIR/asn1crt.c \
    $OUT_DIR/asn1crt_encoding.c \
    $OUT_DIR/asn1crt_encoding_xer.c \
    -I$OUT_DIR

if [ $? -ne 0 ]; then
    echo "Error: Compilation failed."
    exit 1
fi

# 5. Run Test
echo "Step 5: Running test..."
./$RUNNER_EXE
RET_CODE=$?

if [ $RET_CODE -eq 0 ]; then
    echo "Test Result: PASS"
else
    echo "Test Result: FAIL"
fi

exit $RET_CODE
