#!/bin/bash
# Security regression test - Case 4: XER AddAttribute() unbounded write (ESACERT #74508)
#
# Before the fix, Xer_DecodeAttributes() looped over the XML attributes of an
# element without any cap and AddAttribute() guarded the fixed-capacity
# XmlAttributeArray (20 entries) only with an assert(), which release builds
# (-DNDEBUG) compile out. Two failure modes were reachable:
#   * generated decoders always pass a NULL attribute array, so any attribute in
#     the input dereferenced NULL inside AddAttribute() (crash);
#   * a caller that provides its own XmlAttributeArray (the reporter's scenario)
#     got a linear overflow of attacker-controlled bytes past the array once
#     more than 20 attributes were supplied, plus an intra-object overflow of
#     Name[50] by attribute names longer than 49 characters.
# After the fix both paths fail closed: the decoder returns FALSE with
# ERR_INVALID_XML_FILE and never writes outside the array.
#
# The test is compiled with -DNDEBUG to emulate release/flight builds and with
# AddressSanitizer when the compiler supports it. Every sub-test runs in its
# own process so that a crash in one of them cannot mask the others.

cd "$(dirname "$0")" || exit 1

ASN1SCC="${ASN1SCC:-}"
if [ -z "$ASN1SCC" ]; then
    for c in asn1scc asn1scc.exe; do
        if command -v "$c" >/dev/null 2>&1; then ASN1SCC="$c"; break; fi
    done
fi
if [ -z "$ASN1SCC" ]; then
    echo "Error: asn1scc not found on PATH (set ASN1SCC=/path/to/asn1scc)."
    exit 1
fi

OUT_DIR="c_out"
RUNNER="runner.c"
RUNNER_EXE="runner"

rm -rf "$OUT_DIR" "$RUNNER" "$RUNNER_EXE"

echo "Step 1: Generating C code with XER support..."
mkdir -p "$OUT_DIR"
"$ASN1SCC" -XER -c -o "$OUT_DIR" a.asn || { echo "Error: asn1scc failed."; exit 1; }

echo "Step 2: Selecting compiler flags (release build emulation)..."
CFLAGS="-g -O0 -DNDEBUG -Wall"
if echo 'int main(void){return 0;}' | gcc -fsanitize=address -x c - -o /dev/null 2>/dev/null; then
    CFLAGS="$CFLAGS -fsanitize=address -fno-omit-frame-pointer"
    echo "  AddressSanitizer: enabled"
else
    echo "  AddressSanitizer: not available, running without it"
fi

echo "Step 3: Creating runner source code..."
cat << 'CEOF' > "$RUNNER"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "asn1crt.h"
#include "asn1crt_encoding.h"
#include "asn1crt_encoding_xer.h"
#include "a.h"

/* Internal runtime function exercised directly, exactly as in the reporter's PoC. */
extern flag Xer_DecodeAttributes(ByteStream* pByteStrm, XmlAttributeArray* pAttrs, int* pErrCode);

static char xml[16384];

static void appendAttributes(int n) {
    int i;
    for (i = 0; i < n; i++)
        sprintf(xml + strlen(xml), " attr%d=\"%d\"", i, i);
}

/* Decode xml[] with the generated PDU decoder. */
static int generatedDecode(const char* name, flag expectRet) {
    PDU pdu;
    ByteStream strm;
    int errCode = 0;
    flag ret;

    PDU_Initialize(&pdu);
    ByteStream_AttachBuffer(&strm, (unsigned char*)xml, (long)strlen(xml));
    ret = PDU_XER_Decode(&pdu, &strm, &errCode);
    printf("%-28s ret=%d errCode=%d a=%lld b=%lld -> ", name, (int)ret, errCode, (long long)pdu.a, (long long)pdu.b);
    if (ret != expectRet) { printf("FAIL\n"); return 1; }
    if (ret && (pdu.a != 1 || pdu.b != 2)) { printf("FAIL (wrong content)\n"); return 1; }
    printf("PASS\n");
    return 0;
}

/* Call Xer_DecodeAttributes() directly on xml[] (positioned right after the element tag). */
static int directDecode(const char* name, XmlAttributeArray* pAttrs, flag expectRet, int expectCount) {
    ByteStream strm;
    int errCode = 0;
    flag ret;
    int n;

    if (pAttrs != NULL) memset(pAttrs, 0, sizeof(*pAttrs));
    ByteStream_AttachBuffer(&strm, (unsigned char*)xml, (long)strlen(xml));
    ret = Xer_DecodeAttributes(&strm, pAttrs, &errCode);
    n = pAttrs != NULL ? pAttrs->nCount : -1;
    printf("%-28s ret=%d errCode=%d nCount=%d -> ", name, (int)ret, errCode, n);
    if (ret != expectRet) { printf("FAIL\n"); return 1; }
    if (pAttrs != NULL && (n < 0 || n > 20)) { printf("FAIL (nCount out of range)\n"); return 1; }
    if (expectCount >= 0 && n != expectCount) { printf("FAIL (nCount %d expected %d)\n", n, expectCount); return 1; }
    printf("PASS\n");
    return 0;
}

int main(int argc, char** argv) {
    XmlAttributeArray attrs;           /* on the stack, as in the reporter's PoC */
    const char* t = argc > 1 ? argv[1] : "";
    int i;
    xml[0] = 0;

    /* Generated-decoder path (the attribute array is always NULL there). */
    if (strcmp(t, "G1") == 0) {        /* positive control: well-formed document decodes */
        strcpy(xml, "<PDU><a>1</a><b>2</b></PDU>");
        return generatedDecode("G1-generated-valid", TRUE);
    }
    if (strcmp(t, "G2") == 0) {        /* one attribute: NULL dereference before the fix */
        strcpy(xml, "<PDU"); appendAttributes(1); strcat(xml, "><a>1</a><b>2</b></PDU>");
        return generatedDecode("G2-generated-1-attr", FALSE);
    }
    if (strcmp(t, "G3") == 0) {        /* 40 attributes */
        strcpy(xml, "<PDU"); appendAttributes(40); strcat(xml, "><a>1</a><b>2</b></PDU>");
        return generatedDecode("G3-generated-40-attrs", FALSE);
    }

    /* Direct runtime path with caller-provided storage. */
    if (strcmp(t, "D1") == 0) {        /* 40 attributes > capacity 20: linear stack overflow before the fix */
        appendAttributes(40); strcat(xml, ">");
        return directDecode("D1-direct-40-attrs", &attrs, FALSE, -1);
    }
    if (strcmp(t, "D2") == 0) {        /* 90-character attribute name: Name[50] intra-object overflow before the fix */
        strcat(xml, " ");
        for (i = 0; i < 90; i++) strcat(xml, "n");
        strcat(xml, "=\"1\">");
        return directDecode("D2-direct-long-name", &attrs, FALSE, -1);
    }
    if (strcmp(t, "D3") == 0) {        /* exactly 20 attributes must still fit */
        appendAttributes(20); strcat(xml, ">");
        return directDecode("D3-direct-20-attrs", &attrs, TRUE, 20);
    }
    if (strcmp(t, "D4") == 0) {        /* positive control: contents stored correctly */
        strcpy(xml, " x=\"1\" y=\"22\" z=\"333\">");
        if (directDecode("D4-direct-3-attrs", &attrs, TRUE, 3) != 0) return 1;
        if (strcmp(attrs.attrs[0].Name, "x") != 0 || strcmp(attrs.attrs[0].Value, "1") != 0 ||
            strcmp(attrs.attrs[2].Name, "z") != 0 || strcmp(attrs.attrs[2].Value, "333") != 0) {
            printf("D4 content mismatch -> FAIL\n"); return 1;
        }
        return 0;
    }
    if (strcmp(t, "D5") == 0) {        /* no storage provided and one attribute present: must be rejected, not dereferenced */
        appendAttributes(1); strcat(xml, ">");
        return directDecode("D5-direct-null-array", NULL, FALSE, -1);
    }
    fprintf(stderr, "unknown sub-test '%s'\n", t);
    return 2;
}
CEOF

echo "Step 4: Compiling test runner..."
gcc $CFLAGS -o "$RUNNER_EXE" "$RUNNER" \
    "$OUT_DIR/a.c" \
    "$OUT_DIR/asn1crt.c" \
    "$OUT_DIR/asn1crt_encoding.c" \
    "$OUT_DIR/asn1crt_encoding_xer.c" \
    -I"$OUT_DIR" || { echo "Error: Compilation failed."; exit 1; }

echo "Step 5: Running sub-tests (each in its own process)..."
FAILED=0
for t in G1 G2 G3 D1 D2 D3 D4 D5; do
    "./$RUNNER_EXE" "$t"
    rc=$?
    if [ $rc -ne 0 ]; then
        echo "  -> sub-test $t FAILED (exit code $rc)"
        FAILED=$((FAILED + 1))
    fi
done

if [ $FAILED -eq 0 ]; then
    echo "Test Result: PASS"
    exit 0
else
    echo "Test Result: FAIL ($FAILED sub-test(s) failed)"
    exit 1
fi
