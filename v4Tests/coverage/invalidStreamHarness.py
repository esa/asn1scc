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
           "invalid_codes": (51, 0, 1023),
           "read": "return ((unsigned int)buffer[0] << 2) | ((unsigned int)buffer[1] >> 6);",
           "write": """buffer[0] = (byte)(code >> 2);
    buffer[1] = (byte)((buffer[1] & 0x3Fu) | ((code & 3u) << 6));"""}
LAYOUTS = {
    UNIT: {**POS_INT_LAYOUT, "codes": (50, 60),
           "canonical_bytes": ((0x0C, 0x80), (0x0F, 0x00))},
    "04-ENUMERATED/001.asn1#2": {
        "format": "bcd", "bits": 12, "maximum": 999, "codes": (50, 60),
        "invalid_codes": (51, 0, 999),
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
        "invalid_codes": (51, 0, 9999),
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
    "04-ENUMERATED/002.asn1#1": {
        "format": "twos-complement", "bits": 10, "maximum": 511,
        "code_type": "int", "codes": (-1, -200), "invalid_codes": (-2, 0, -512, 511),
        "canonical_bytes": ((0xFF, 0xC0), (0xCE, 0x00)),
        "read": """unsigned int raw = ((unsigned int)buffer[0] << 2) | ((unsigned int)buffer[1] >> 6);
    /* raw is at most 1023, so the cast and signed subtraction are defined. */
    return raw >= 512u ? (int)raw - 1024 : (int)raw;""",
        "write": """/* Convert the signed value to nonnegative wire bits before shifting. */
    unsigned int raw = (unsigned int)(code < 0 ? code + 1024 : code);
    buffer[0] = (byte)(raw >> 2);
    buffer[1] = (byte)((buffer[1] & 0x3Fu) | ((raw & 3u) << 6));"""},
    "04-ENUMERATED/002.asn1#2": {
        "format": "signed-ascii", "bits": 32, "maximum": 999,
        "code_type": "int", "codes": (-1, -200), "invalid_codes": (-2, 0, -999, 999),
        "canonical_bytes": ((0x2D, 0x30, 0x30, 0x31), (0x2D, 0x32, 0x30, 0x30)),
        "invalid_bytes": ((0x2D, 0x30, 0x30, 0x32), (0x2B, 0x30, 0x30, 0x30),
                          (0x2D, 0x39, 0x39, 0x39), (0x2B, 0x39, 0x39, 0x39)),
        "read": """int magnitude = 0;
    if (buffer[0] != 0x2D && buffer[0] != 0x2B) return 1000;
    for (int digit = 1; digit < 4; ++digit) {
        if (buffer[digit] < 0x30 || buffer[digit] > 0x39) return 1000;
        magnitude = magnitude * 10 + buffer[digit] - 0x30;
    }
    return buffer[0] == 0x2D ? -magnitude : magnitude;""",
        "write": """/* Layout values are bounded by +/-999; negation is representable. */
    int magnitude = code < 0 ? -code : code;
    buffer[0] = code < 0 ? 0x2D : 0x2B;
    for (int digit = 3; digit >= 1; --digit) {
        buffer[digit] = (byte)(0x30 + magnitude % 10);
        magnitude /= 10;
    }"""},
}
SUPPORTED_UNITS = tuple(LAYOUTS)


def attached_bytes(unit):
    return (LAYOUTS[unit]["bits"] + 7) // 8


def padding_bits(unit):
    return attached_bytes(unit) * 8 - LAYOUTS[unit]["bits"]


def cases_for(unit):
    return (("original",) + tuple(f"code-{code}" for code in LAYOUTS[unit]["invalid_codes"])
            + ("other-valid",) + (("padding-only",) if padding_bits(unit) else ()))


def success_for(unit):
    return tuple(case in ("original", "other-valid", "padding-only") for case in cases_for(unit))

DRIVER_TEMPLATE = r'''
#include <stdio.h>
#include <string.h>
#include "sample1.h"

static @CODE_TYPE@ coverage_wire_code(const byte *buffer)
{
    @READ@
}

static void coverage_write_code(byte *buffer, @CODE_TYPE@ code)
{
    @WRITE@
}

static int coverage_invalid_stream_checks(void)
{
    static const ASN1SCC_MyPDU seeds[] = {MyPDU_alpha, MyPDU_beta};
    static const @CODE_TYPE@ codes[] = {@CODES@};
    static const byte canonical[2][@BYTES@] = {@CANONICAL@};
    static const char *seed_names[] = {"alpha", "beta"};
    static const char *cases[] = {@CASES@};
    static const int success[] = {@SUCCESS@};
    for (int seed = 0; seed < 2; ++seed) {
        byte encoded[@BYTES@] = {0};
        BitStream encode_stream;
        int encode_error = 0;
        BitStream_Init(&encode_stream, encoded, sizeof encoded);
        if (ASN1SCC_MyPDU_REQUIRED_BYTES_FOR_ACN_ENCODING != @BYTES@
            || !ASN1SCC_MyPDU_ACN_Encode(&seeds[seed], &encode_stream, &encode_error, TRUE)
            || encode_error != 0 || BitStream_GetLength(&encode_stream) != @BYTES@
            || encode_stream.currentByte * 8 + encode_stream.currentBit != @BITS@
            || coverage_wire_code(encoded) != codes[seed]
            || memcmp(encoded, canonical[seed], sizeof encoded) != 0) {
            puts("Invalid stream seed: unexpected encoding/layout");
            return 1;
        }
        const @CODE_TYPE@ replacements[] = {@REPLACEMENTS@};@WIRE_EXPECTATIONS@
        for (int test = 0; test < @CASE_COUNT@; ++test) {
            byte mutated[@BYTES@];
            byte saved[@BYTES@];
            BitStream stream;
            memcpy(mutated, encoded, sizeof mutated);
            if (test >= 1 && test <= @OTHER_VALID@) {
                /* Replace only the field, preserving any rounded-byte padding. */
                coverage_write_code(mutated, replacements[test]);
            }
            @PAD_MUTATION@
            if (coverage_wire_code(mutated) != replacements[test]@WIRE_CHECK@@PAD_CHECK@) {
                puts("Invalid stream mutation: unexpected field/padding change");
                return 1;
            }
            memcpy(saved, mutated, sizeof saved);
            BitStream_AttachBuffer(&stream, mutated, sizeof mutated);
            /* A distinct initial output makes missing decoder assignments visible. */
            ASN1SCC_MyPDU expected = test == @OTHER_VALID@ ? seeds[1 - seed] : seeds[seed];
            ASN1SCC_MyPDU decoded = expected == MyPDU_alpha ? MyPDU_beta : MyPDU_alpha;
            int error = 0;
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
    other_valid = cases.index("other-valid")
    padding_case = cases.index("padding-only") if padding else None
    replacements = ["codes[seed]", *map(str, layout["invalid_codes"]), "codes[1 - seed]"]
    if padding:
        replacements.append("codes[seed]")
    wire_expectations = wire_check = ""
    if "invalid_bytes" in layout:
        rows = layout["invalid_bytes"]
        assert len(rows) == len(layout["invalid_codes"]) and not padding
        data = ", ".join("{" + ", ".join(hex(b) for b in row) + "}" for row in rows)
        pointers = ["canonical[seed]", *[f"invalid_canonical[{i}]" for i in range(len(rows))],
                    "canonical[1 - seed]"]
        wire_expectations = (f"\n        static const byte invalid_canonical[{len(rows)}][{size}] = {{{data}}};"
                             f"\n        const byte *expected_wire[] = {{{', '.join(pointers)}}};")
        wire_check = "\n                || memcmp(mutated, expected_wire[test], sizeof mutated) != 0"
    substitutions = {"READ": layout["read"], "WRITE": layout["write"],
                     "WIRE_EXPECTATIONS": wire_expectations, "WIRE_CHECK": wire_check,
                     "CODE_TYPE": layout.get("code_type", "unsigned int"),
                     "OTHER_VALID": str(other_valid),
                     "CODES": ", ".join(map(str, layout["codes"])),
                     "CANONICAL": ", ".join("{" + ", ".join(hex(b) for b in row) + "}"
                                            for row in layout["canonical_bytes"]),
                     "BITS": str(layout["bits"]), "BYTES": str(size),
                     "CASE_COUNT": str(len(cases)), "DECODE_COUNT": str(len(SEEDS) * len(cases)),
                     "REPLACEMENTS": ", ".join(replacements),
                     "SUCCESS": ", ".join(str(int(success)) for success in success_for(unit)),
                     "PAD_MUTATION": f"if (test == {padding_case}) mutated[{last}] ^= 1u;" if padding else "",
                     "PAD_CHECK": (f"\n                || (test != {padding_case} && (mutated[{last}] & {mask}) != (encoded[{last}] & {mask}))"
                                   f"\n                || (test == {padding_case} && (mutated[{last}] ^ encoded[{last}]) != 1u)") if padding else "",
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
            "wire_codes": list(layout["codes"]), "invalid_codes": list(layout["invalid_codes"]),
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
