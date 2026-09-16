"""Bounded, explicit C prefix oracles. This is not a generic truncation oracle."""
import hashlib
from pathlib import Path
import re


CASES = {
    "10-SEQEUENCE/008.asn1#1": {
        "encodings": "uper", "type": "MyPDU", "codec": "", "value": "pdu1", "bytes": 16,
        "prefixes": [0, 1, 3, 15], "counts": [-1, -1, -1, -1], "bits": [0, 0, 0, 0],
        "errors": ["ERR_UPER_DECODE_MYPDU", "ERR_UPER_DECODE_MYPDU_INT2",
                   "ERR_UPER_DECODE_MYPDU_ENM", "ERR_UPER_DECODE_MYPDU_GG_INT2"],
    },
    "24-DEDUCED-SIZE/002.asn1#1": {
        "encodings": "acn", "type": "EList", "codec": "ACN_", "value": "lst1", "bytes": 8,
        "prefixes": [0, 1, 2, 5], "counts": [0, -1, -1, 2], "bits": [0, 0, 0, 34],
        "errors": ["0", "ERR_ACN_DECODE_ELIST", "ERR_ACN_DECODE_ELEM_B", "0"],
    },
    "24-DEDUCED-SIZE/001.asn1#2": {
        "encodings": "acn", "type": "TLVList", "codec": "ACN_", "value": "lst1", "bytes": 6,
        "prefixes": [0, 1, 2, 5], "counts": [0, -1, 1, -1], "bits": [0, 0, 16, 0],
        "errors": ["0", "ERR_ACN_DECODE_TLVLIST", "0", "ERR_ACN_DECODE_TLVLIST"],
    },
}


def driver(case):
    tas = "ASN1SCC_" + case["type"]
    codec = tas + "_" + case["codec"]
    success = """
        TYPE expected = VALUE;
        expected.nCount = expected_counts[i];
        int validation_error = 0;
        oracle = oracle && TYPE_IsConstraintValid(&decoded, &validation_error)
            && TYPE_Equal(&expected, &decoded)
            && view.currentByte * 8 + view.currentBit == expected_bits[i];
""" if case["type"] != "MyPDU" else ""
    fallback = """
    if (prefixes[i] == 3) oracle = oracle && decoded.enm == MyPDU_enm_one;
""" if case["type"] == "MyPDU" else ""
    body = r'''
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "sample1.h"
#if defined(__SANITIZE_ADDRESS__)
#include <sanitizer/asan_interface.h>
#endif

static int coverage_decode_probes(void)
{
    static const int prefixes[] = {PREFIXES};
    static const int expected_counts[] = {COUNTS};
    static const int expected_errors[] = {ERRORS};
    BITS_DECL
    byte encoded[TYPE_REQUIRED_BYTES_FOR_ENCODING + 1] = {0};
    BitStream full;
    int error = 0;
    BitStream_Init(&full, encoded, (long)sizeof encoded);
    if (!CODECEncode(&VALUE, &full, &error, TRUE) || error != 0
        || BitStream_GetLength(&full) != EXPECTED_BYTES) return 1;
    for (int i = 0; i < 4; ++i) {
        TYPE decoded;
        memset(&decoded, 0, sizeof decoded);
        INITIAL_OUTPUT
        /* Exact allocation exposes reads outside the declared view to ASan. */
        byte *prefix = malloc(prefixes[i] == 0 ? 1 : (size_t)prefixes[i]);
        if (prefix == NULL) return 1;
        if (prefixes[i] != 0) memcpy(prefix, encoded, (size_t)prefixes[i]);
#if defined(__SANITIZE_ADDRESS__)
        if (prefixes[i] == 0) __asan_poison_memory_region(prefix, 1);
#endif
        BitStream view;
        BitStream_AttachBuffer(&view, prefix, prefixes[i]);
        error = 0;
        flag accepted = CODECDecode(&decoded, &view, &error);
        int oracle = (accepted == (expected_counts[i] >= 0)) && error == expected_errors[i];
        if (accepted) {
SUCCESS
        }
FALLBACK
#if defined(__SANITIZE_ADDRESS__)
        if (prefixes[i] == 0) __asan_unpoison_memory_region(prefix, 1);
#endif
        free(prefix);
        if (!oracle) {
            printf("Decode prefix %d failed: accepted=%d error=%d expected=%d/%d\n",
                   prefixes[i], accepted, error, expected_counts[i] >= 0, expected_errors[i]);
            return 1;
        }
        printf("Decode prefix %d: expected %s, OK\n", prefixes[i], accepted ? "success" : "failure");
    }
    puts("Decode prefix checks (4) run successfully.");
    return 0;
}
'''
    replacements = {
        "SUCCESS": success, "FALLBACK": fallback,
        "INITIAL_OUTPUT": "decoded.enm = MyPDU_enm_thousand;" if case["type"] == "MyPDU" else "",
        "PREFIXES": ", ".join(map(str, case["prefixes"])),
        "COUNTS": ", ".join(map(str, case["counts"])), "ERRORS": ", ".join(case["errors"]),
        "BITS_DECL": ("static const int expected_bits[] = {" + ", ".join(map(str, case["bits"])) + "};")
                     if success else "",
        "EXPECTED_BYTES": str(case["bytes"]),
        "TYPE_REQUIRED_BYTES_FOR_ENCODING": tas + "_REQUIRED_BYTES_FOR_" + case["codec"] + "ENCODING",
        "CODEC": codec, "TYPE": tas, "VALUE": case["value"],
    }
    for old, new in replacements.items():
        body = body.replace(old, new)
    return body


def prepare(work, unit, args):
    stage = args.decode_stage
    result = {"stage": stage, "prefix_checks": 0}
    if stage == "baseline":
        return result
    harness = list(work.glob("*_auto_tcs.c"))
    if not harness or not any("#ifdef ASN1SCC_DECODE_ACTUAL_LENGTH" in p.read_text() for p in harness):
        raise ValueError("Generated harness does not support actual-length decode")
    if stage == "actual":
        return result
    case = CASES.get(unit)
    if case is None or args.encodings != case["encodings"]:
        raise ValueError("No explicit truncation oracle for this unit/encoding")
    path = work / "mainprogram.c"
    text = path.read_text()
    old = "return asn1scc_run_generated_testsuite(&output);"
    if text.count(old) != 1:
        raise ValueError("Unexpected generated test runner")
    new = "int positives = asn1scc_run_generated_testsuite(&output);\n    if (positives != 0) return positives;\n    return coverage_decode_probes();"
    source = driver(case)
    text = text.replace(old, new)
    path.write_text(source + "\n" + text)
    return {**result, "prefix_checks": 4, "oracle": case,
            "driver_sha256": hashlib.sha256(source.encode()).hexdigest()}


def verify_output(output, checks):
    if checks["stage"] == "baseline":
        return
    positive = re.search(r"All test cases \((\d+)\) run successfully", output)
    if not positive or int(positive[1]) == 0:
        raise ValueError("Missing original positive test executions")
    if checks["prefix_checks"]:
        if output.count("Decode prefix checks (4) run successfully.") != 1:
            raise ValueError("Missing bounded prefix checks")
        for prefix, count in zip(checks["oracle"]["prefixes"], checks["oracle"]["counts"]):
            expected = "success" if count >= 0 else "failure"
            if output.count(f"Decode prefix {prefix}: expected {expected}, OK") != 1:
                raise ValueError("Missing or unexpected prefix outcome")
