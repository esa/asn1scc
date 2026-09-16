"""Fixed-layout ACN enum mutations; no generic invalid-stream oracle is implied."""
import hashlib
import re

UNIT = "04-ENUMERATED/001.asn1#1"
SEEDS = ("alpha", "beta")
CASES = ("original", "code-51", "code-0", "code-1023", "other-valid", "padding-only")
SUCCESS = (True, False, False, False, True, True)

DRIVER = r'''
#include <stdio.h>
#include <string.h>
#include "sample1.h"

static unsigned int coverage_wire_code(const byte *buffer)
{
    return ((unsigned int)buffer[0] << 2) | ((unsigned int)buffer[1] >> 6);
}

static int coverage_invalid_stream_checks(void)
{
    static const ASN1SCC_MyPDU seeds[] = {MyPDU_alpha, MyPDU_beta};
    static const unsigned int codes[] = {50, 60};
    static const char *seed_names[] = {"alpha", "beta"};
    static const char *cases[] = {"original", "code-51", "code-0", "code-1023", "other-valid", "padding-only"};
    static const int success[] = {1, 0, 0, 0, 1, 1};
    for (int seed = 0; seed < 2; ++seed) {
        byte encoded[2] = {0};
        BitStream stream;
        int error = 0;
        BitStream_Init(&stream, encoded, sizeof encoded);
        if (ASN1SCC_MyPDU_REQUIRED_BYTES_FOR_ACN_ENCODING != 2
            || !ASN1SCC_MyPDU_ACN_Encode(&seeds[seed], &stream, &error, TRUE)
            || error != 0 || BitStream_GetLength(&stream) != 2
            || stream.currentByte * 8 + stream.currentBit != 10
            || coverage_wire_code(encoded) != codes[seed]) {
            puts("Invalid stream seed: unexpected encoding/layout");
            return 1;
        }
        const unsigned int replacements[] = {codes[seed], 51, 0, 1023, codes[1 - seed], codes[seed]};
        for (int test = 0; test < 6; ++test) {
            byte mutated[2];
            byte saved[2];
            memcpy(mutated, encoded, sizeof mutated);
            if (test >= 1 && test <= 4) {
                /* Field starts at bit 0, width 10, MSB first; preserve six padding bits. */
                mutated[0] = (byte)(replacements[test] >> 2);
                mutated[1] = (byte)((mutated[1] & 0x3Fu) | ((replacements[test] & 3u) << 6));
            }
            if (test == 5) mutated[1] ^= 1u;
            if (coverage_wire_code(mutated) != replacements[test]
                || (test != 5 && (mutated[1] & 0x3Fu) != (encoded[1] & 0x3Fu))
                || (test == 5 && (mutated[1] ^ encoded[1]) != 1u)) {
                puts("Invalid stream mutation: unexpected field/padding change");
                return 1;
            }
            memcpy(saved, mutated, sizeof saved);
            BitStream_AttachBuffer(&stream, mutated, sizeof mutated);
            /* A distinct initial output makes missing decoder assignments visible. */
            ASN1SCC_MyPDU expected = test == 4 ? seeds[1 - seed] : seeds[seed];
            ASN1SCC_MyPDU decoded = expected == MyPDU_alpha ? MyPDU_beta : MyPDU_alpha;
            error = 0;
            flag accepted = ASN1SCC_MyPDU_ACN_Decode(&decoded, &stream, &error);
            if (accepted != success[test] || error != (success[test] ? 0 : ERR_ACN_DECODE_MYPDU)
                || stream.currentByte * 8 + stream.currentBit != 10 || stream.count != 2
                || memcmp(mutated, saved, sizeof mutated) != 0
                || (accepted && decoded != expected)) {
                printf("Invalid stream %s/%s: unexpected result/error/value/stream\n", seed_names[seed], cases[test]);
                return 1;
            }
            printf("Invalid stream %s/%s: expected %s, OK\n", seed_names[seed], cases[test],
                   success[test] ? "success" : "rejection");
        }
    }
    puts("Invalid stream checks (12) run successfully.");
    return 0;
}
'''


def target_lines(work):
    lines = (work / "sample1.c").read_text().splitlines()
    starts = [i for i, line in enumerate(lines) if line.startswith("flag ASN1SCC_MyPDU_ACN_Decode(")]
    if len(starts) != 1:
        raise ValueError("Missing or ambiguous target decoder")
    start = starts[0]
    stop = next((i for i in range(start + 1, len(lines)) if lines[i] == "}"), None)
    if stop is None:
        raise ValueError("Unterminated target decoder")
    matches = [i for i in range(start, stop) if lines[i].strip().startswith("ret = FALSE;")]
    if len(matches) != 1 or not lines[matches[0] + 1].strip().startswith("*pErrCode = ERR_ACN_DECODE_MYPDU;"):
        raise ValueError("Expected two explicit decoder-rejection statements")
    return [matches[0] + 1, matches[0] + 2]


def prepare(work, unit, args):
    if unit != UNIT or args.language != "c" or args.encodings != "acn":
        raise ValueError("No explicit invalid-stream oracle for this unit/language/encoding")
    targets = target_lines(work)
    path = work / "mainprogram.c"
    text = path.read_text()
    old = "return asn1scc_run_generated_testsuite(&output);"
    if text.count(old) != 1:
        raise ValueError("Unexpected generated test runner")
    replacement = "int positives = asn1scc_run_generated_testsuite(&output);\n    if (positives != 0) return positives;\n    return coverage_invalid_stream_checks();"
    path.write_text(DRIVER + "\n" + text.replace(old, replacement))
    return {"seeds": list(SEEDS), "cases": list(CASES), "success": list(SUCCESS),
            "wire_codes": [50, 60], "invalid_codes": [51, 0, 1023],
            "field_offset_bits": 0, "field_width_bits": 10, "attached_bytes": 2,
            "encode_calls": 2, "decode_calls": 12, "negative_decodes": 6,
            "target_lines": targets, "driver_sha256": hashlib.sha256(DRIVER.encode()).hexdigest()}


def verify_output(output, checks):
    positive = re.search(r"All test cases \((\d+)\) run successfully", output)
    if not positive or int(positive[1]) == 0:
        raise ValueError("Missing original positive tests")
    if output.count("Invalid stream checks (12) run successfully.") != 1:
        raise ValueError("Missing invalid-stream checks")
    for seed in checks["seeds"]:
        for case, success in zip(checks["cases"], checks["success"]):
            expected = f"Invalid stream {seed}/{case}: expected {'success' if success else 'rejection'}, OK"
            if output.count(expected) != 1:
                raise ValueError("Missing or unexpected invalid-stream outcome: " + expected)
