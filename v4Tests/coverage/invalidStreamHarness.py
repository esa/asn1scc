"""Fixed-layout ACN enum mutations; no generic invalid-stream oracle is implied."""
import hashlib
import json
import re

UNIT = "04-ENUMERATED/001.asn1#1"
SEEDS = ("alpha", "beta")
CASES = ("original", "code-51", "code-0", "code-1023", "other-valid", "padding-only")
SUCCESS = (True, False, False, False, True, True)

# Only explicitly described wire layouts have an oracle. Decimal bounds ensure
# every negative reaches the generated enum switch, not RTL digit rejection.
POS_INT_LAYOUT = {"format": "pos-int", "bits": 10, "maximum": 1023,
           "read": "return ((unsigned int)buffer[0] << 2) | ((unsigned int)buffer[1] >> 6);",
           "write": """buffer[0] = (byte)(code >> 2);
    buffer[1] = (byte)((buffer[1] & 0x3Fu) | ((code & 3u) << 6));"""}
LAYOUTS = {
    UNIT: {**POS_INT_LAYOUT, "codes": (50, 60),
           "canonical_bytes": ((0x0C, 0x80), (0x0F, 0x00))},
    "04-ENUMERATED/001.asn1#2": {
        "format": "bcd", "bits": 12, "maximum": 999, "codes": (50, 60),
        "canonical_bytes": ((0x05, 0x00), (0x06, 0x00)),
        "read": """unsigned int hundreds = buffer[0] >> 4;
    unsigned int tens = buffer[0] & 15u;
    unsigned int ones = buffer[1] >> 4;
    if (hundreds > 9 || tens > 9 || ones > 9) return 1000;
    return hundreds * 100 + tens * 10 + ones;""",
        "write": """buffer[0] = (byte)(((code / 100) << 4) | ((code / 10) % 10));
    buffer[1] = (byte)((buffer[1] & 0x0Fu) | ((code % 10) << 4));"""},
    "04-ENUMERATED/001.asn1#5": {
        **POS_INT_LAYOUT, "codes": (1, 200),
        "canonical_bytes": ((0x00, 0x40), (0x32, 0x00))},
    "04-ENUMERATED/001.asn1#3": {
        "format": "ascii", "bits": 32, "maximum": 9999, "codes": (50, 60),
        "canonical_bytes": ((0x30, 0x30, 0x35, 0x30), (0x30, 0x30, 0x36, 0x30)),
        "read": """unsigned int code = 0;
    for (int digit = 0; digit < 4; ++digit) {
        if (buffer[digit] < 0x30 || buffer[digit] > 0x39) return 10000;
        code = code * 10 + buffer[digit] - 0x30;
    }
    return code;""",
        "write": """for (int digit = 3; digit >= 0; --digit) {
        buffer[digit] = (byte)(0x30 + code % 10);
        code /= 10;
    }"""},
}
SUPPORTED_UNITS = tuple(LAYOUTS)


def attached_bytes(unit):
    return (LAYOUTS[unit]["bits"] + 7) // 8


def padding_bits(unit):
    return attached_bytes(unit) * 8 - LAYOUTS[unit]["bits"]


def cases_for(unit):
    return ("original", "code-51", "code-0", f"code-{LAYOUTS[unit]['maximum']}",
            "other-valid") + (("padding-only",) if padding_bits(unit) else ())


def success_for(unit):
    return tuple(case in ("original", "other-valid", "padding-only") for case in cases_for(unit))

DRIVER_TEMPLATE = r'''
#include <stdio.h>
#include <string.h>
#include "sample1.h"

static unsigned int coverage_wire_code(const byte *buffer)
{
    @READ@
}

static void coverage_write_code(byte *buffer, unsigned int code)
{
    @WRITE@
}

static int coverage_invalid_stream_checks(void)
{
    static const ASN1SCC_MyPDU seeds[] = {MyPDU_alpha, MyPDU_beta};
    static const unsigned int codes[] = {@CODES@};
    static const byte canonical[2][@BYTES@] = {@CANONICAL@};
    static const char *seed_names[] = {"alpha", "beta"};
    static const char *cases[] = {@CASES@};
    static const int success[] = {@SUCCESS@};
    for (int seed = 0; seed < 2; ++seed) {
        byte encoded[@BYTES@] = {0};
        BitStream stream;
        int error = 0;
        BitStream_Init(&stream, encoded, sizeof encoded);
        if (ASN1SCC_MyPDU_REQUIRED_BYTES_FOR_ACN_ENCODING != @BYTES@
            || !ASN1SCC_MyPDU_ACN_Encode(&seeds[seed], &stream, &error, TRUE)
            || error != 0 || BitStream_GetLength(&stream) != @BYTES@
            || stream.currentByte * 8 + stream.currentBit != @BITS@
            || coverage_wire_code(encoded) != codes[seed]
            || memcmp(encoded, canonical[seed], sizeof encoded) != 0) {
            puts("Invalid stream seed: unexpected encoding/layout");
            return 1;
        }
        const unsigned int replacements[] = {@REPLACEMENTS@};
        for (int test = 0; test < @CASE_COUNT@; ++test) {
            byte mutated[@BYTES@];
            byte saved[@BYTES@];
            memcpy(mutated, encoded, sizeof mutated);
            if (test >= 1 && test <= 4) {
                /* Replace only the field, preserving any rounded-byte padding. */
                coverage_write_code(mutated, replacements[test]);
            }
            @PAD_MUTATION@
            if (coverage_wire_code(mutated) != replacements[test]@PAD_CHECK@) {
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
                || stream.currentByte * 8 + stream.currentBit != @BITS@ || stream.count != @BYTES@
                || memcmp(mutated, saved, sizeof mutated) != 0
                || (accepted && decoded != expected)) {
                printf("Invalid stream %s/%s: unexpected result/error/value/stream\n", seed_names[seed], cases[test]);
                return 1;
            }
            printf("Invalid stream %s/%s: expected %s, OK\n", seed_names[seed], cases[test],
                   success[test] ? "success" : "rejection");
        }
    }
    puts("Invalid stream checks (@DECODE_COUNT@) run successfully.");
    return 0;
}
'''


def driver_for(unit):
    layout = LAYOUTS[unit]
    cases = cases_for(unit)
    size = attached_bytes(unit)
    padding = padding_bits(unit)
    mask = hex((1 << padding) - 1) + "u"
    last = size - 1
    replacements = ["codes[seed]", "51", "0", str(layout["maximum"]), "codes[1 - seed]"]
    if padding:
        replacements.append("codes[seed]")
    substitutions = {"READ": layout["read"], "WRITE": layout["write"],
                     "CODES": ", ".join(map(str, layout["codes"])),
                     "CANONICAL": ", ".join("{" + ", ".join(hex(b) for b in row) + "}"
                                            for row in layout["canonical_bytes"]),
                     "BITS": str(layout["bits"]), "BYTES": str(size),
                     "CASE_COUNT": str(len(cases)), "DECODE_COUNT": str(len(SEEDS) * len(cases)),
                     "REPLACEMENTS": ", ".join(replacements),
                     "SUCCESS": ", ".join(str(int(success)) for success in success_for(unit)),
                     "PAD_MUTATION": f"if (test == 5) mutated[{last}] ^= 1u;" if padding else "",
                     "PAD_CHECK": (f"\n                || (test != 5 && (mutated[{last}] & {mask}) != (encoded[{last}] & {mask}))"
                                   f"\n                || (test == 5 && (mutated[{last}] ^ encoded[{last}]) != 1u)") if padding else "",
                     "CASES": ", ".join(map(json.dumps, cases))}
    driver = DRIVER_TEMPLATE
    for key, value in substitutions.items():
        driver = driver.replace("@" + key + "@", value)
    return driver


# Preserve the original single-unit entrypoint/constants for callers.
DRIVER = driver_for(UNIT)


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
    if unit not in SUPPORTED_UNITS or args.language != "c" or args.encodings != "acn":
        raise ValueError("No explicit invalid-stream oracle for this unit/language/encoding")
    targets = target_lines(work)
    path = work / "mainprogram.c"
    text = path.read_text()
    old = "return asn1scc_run_generated_testsuite(&output);"
    if text.count(old) != 1:
        raise ValueError("Unexpected generated test runner")
    replacement = "int positives = asn1scc_run_generated_testsuite(&output);\n    if (positives != 0) return positives;\n    return coverage_invalid_stream_checks();"
    layout = LAYOUTS[unit]
    driver = driver_for(unit)
    path.write_text(driver + "\n" + text.replace(old, replacement))
    success = success_for(unit)
    return {"seeds": list(SEEDS), "cases": list(cases_for(unit)), "success": list(success),
            "wire_codes": list(layout["codes"]), "invalid_codes": [51, 0, layout["maximum"]],
            "wire_format": layout["format"],
            "field_offset_bits": 0, "field_width_bits": layout["bits"], "attached_bytes": attached_bytes(unit),
            "padding_bits": padding_bits(unit),
            "encode_calls": len(SEEDS), "decode_calls": len(SEEDS) * len(success),
            "negative_decodes": len(SEEDS) * success.count(False),
            "positive_decodes": len(SEEDS) * success.count(True),
            "target_lines": targets, "driver_sha256": hashlib.sha256(driver.encode()).hexdigest()}


def verify_output(output, checks):
    positive = re.search(r"All test cases \((\d+)\) run successfully", output)
    if not positive or int(positive[1]) == 0:
        raise ValueError("Missing original positive tests")
    count = len(checks["seeds"]) * len(checks["cases"])
    if len(checks["cases"]) != len(checks["success"]) or checks["decode_calls"] != count:
        raise ValueError("Inconsistent invalid-stream check accounting")
    if output.count(f"Invalid stream checks ({count}) run successfully.") != 1:
        raise ValueError("Missing invalid-stream checks")
    for seed in checks["seeds"]:
        for case, success in zip(checks["cases"], checks["success"]):
            expected = f"Invalid stream {seed}/{case}: expected {'success' if success else 'rejection'}, OK"
            if output.count(expected) != 1:
                raise ValueError("Missing or unexpected invalid-stream outcome: " + expected)
