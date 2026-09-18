"""Explicit ACN stream profiles, with bounded mutations and exact decode oracles."""
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
# Typed profiles describe values and wire operations separately. The Python wire
# oracle uses integer masks; the generated driver independently writes each bit.
# Only registered, source-grounded profiles are supported (not inferred grammars).
STREAM_PROFILES = {
    "09-CHOICE/013.asn1#1": {
        "type": "ASN1SCC_MyPDU",
        "target_error": "ERR_ACN_DECODE_MYPDU_PAYLOAD",
        "fields": ((4, 8), (12, 8)),
        "invalid_fields": ((18, 1), (20, 2), (0, 0), (255, 255)),
        "rejection_bits": 28,
        "common": {"header.flags.arr[0]": "0", "header.sourceId": "0xA5", "crc": "0x5A"},
        "seeds": (
            {"name": "alt-17-1", "bits": 36, "wire": (1, 16, 26, 85, 160),
             "assign": {"payload.kind": "MyPayload_alt_17_1_PRESENT", "payload.u.alt_17_1": "0"},
             "value": {"payload.kind": "MyPayload_alt_17_1_PRESENT"}},
            {"name": "alt-20-1", "bits": 60, "wire": (1, 64, 26, 80, 17, 35, 69, 160),
             "assign": {"payload.kind": "MyPayload_alt_20_1_PRESENT",
                        "payload.u.alt_20_1.parameterIds.nCount": "1",
                        "payload.u.alt_20_1.parameterIds.arr[0]": "0x1234"},
             "value": {"payload.kind": "MyPayload_alt_20_1_PRESENT",
                       "payload.u.alt_20_1.parameterIds.nCount": "1",
                       "payload.u.alt_20_1.parameterIds.arr[0]": "0x1234"}},
        ),
        "value_fault": "decoded.crc ^= 1u;",
    },
    "15-PUS-ParameterPassing/001.asn1#1": {
        "type": "ASN1SCC_MySeq",
        "target_error": "ERR_ACN_DECODE_MYSEQ_COLORDATA",
        "fields": ((0, 8), (8, 8)),
        "invalid_fields": ((31, 10), (30, 11), (50, 10), (255, 255)),
        "rejection_bits": 16,
        # Successful deferred determinant patching leaves this exact RTL error.
        "encode_errors": {"acn": "0", "acn-v2": "ERR_ACN_DET_CONSISTENCY_MISMATCH"},
        "common": {},
        "seeds": (
            {"name": "green", "bits": 20, "wire": (0x1E, 0x0A, 0x20),
             "assign": {"colorData.kind": "COLOR_DATA_green_PRESENT", "colorData.u.green": "3"},
             "value": {"colorData.kind": "COLOR_DATA_green_PRESENT", "colorData.u.green": "3"}},
            {"name": "red", "bits": 26, "wire": (0x1E, 0x14, 0x0A, 0x40),
             "assign": {"colorData.kind": "COLOR_DATA_red_PRESENT", "colorData.u.red": "42"},
             "value": {"colorData.kind": "COLOR_DATA_red_PRESENT", "colorData.u.red": "42"}},
        ),
        "value_fault": "decoded.colorData.kind = COLOR_DATA_NONE;",
    },
    "06-OCTET-STRING/004.asn1#2": {
        "type": "ASN1SCC_MyPDU",
        "target_error": None,  # Branch-only; no new statement obligations.
        "source_faults": {
            "missing-error-assignment": "*pErrCode = ret ? 0 : ERR_ACN_DECODE_MYPDU_A2;",
        },
        "common": {},
        "seeds": (
            {"bits": 40, "wire": (4, 0xAF, 0xBC, 0x45, 0x83),
             "assign": {"a2.nCount": "4", "a2.arr[0]": "0xAF", "a2.arr[1]": "0xBC",
                        "a2.arr[2]": "0x45", "a2.arr[3]": "0x83"},
             "value": {"a2.nCount": "4", "a2.arr[0]": "0xAF", "a2.arr[1]": "0xBC",
                       "a2.arr[2]": "0x45", "a2.arr[3]": "0x83"}},
        ),
        # All mutations retain the seed's five-byte view, including valid-shorter.
        "cases": (
            {"name": "original", "success": True, "bits": 40, "error": "0"},
            {"name": "length0", "fields": (((0, 8), 0),),
             "success": False, "bits": 8, "error": "0"},
            {"name": "length21", "fields": (((0, 8), 21),),
             "success": False, "bits": 8, "error": "ERR_ACN_DECODE_MYPDU_A2"},
            {"name": "length255", "fields": (((0, 8), 255),),
             "success": False, "bits": 8, "error": "ERR_ACN_DECODE_MYPDU_A2"},
            {"name": "valid-shorter", "fields": (((0, 8), 1),),
             "success": True, "bits": 16, "error": "0",
             "value": {"a2.nCount": "1", "a2.arr[0]": "0xAF"}},
        ),
        "value_fault": "decoded.a2.arr[0] ^= 1u;",
        "length_field": (0, 8),
    },
    "06-OCTET-STRING/004.asn1#1": {
        "type": "ASN1SCC_MyPDU",
        "target_error": None,  # Branch-only; no new statement obligations.
        "source_faults": {
            "missing-error-assignment": "*pErrCode = ret ? 0 : ERR_ACN_DECODE_MYPDU_A2;",
        },
        "common": {},
        "seeds": (
            {"bits": 37, "wire": (0x25, 0x7D, 0xE2, 0x2C, 0x18),
             "assign": {"a2.nCount": "4", "a2.arr[0]": "0xAF", "a2.arr[1]": "0xBC",
                        "a2.arr[2]": "0x45", "a2.arr[3]": "0x83"},
             "value": {"a2.nCount": "4", "a2.arr[0]": "0xAF", "a2.arr[1]": "0xBC",
                       "a2.arr[2]": "0x45", "a2.arr[3]": "0x83"}},
        ),
        # Five determinant bits, unaligned payload, and three real padding bits.
        # Shorter values retain the five-byte view and consume only their payload.
        "cases": (
            {"name": "original", "success": True, "bits": 37, "error": "0"},
            {"name": "length0", "fields": (((0, 5), 0),),
             "success": False, "bits": 5, "error": "0"},
            {"name": "length21", "fields": (((0, 5), 21),),
             "success": False, "bits": 5, "error": "ERR_ACN_DECODE_MYPDU_A2"},
            {"name": "length31", "fields": (((0, 5), 31),),
             "success": False, "bits": 5, "error": "ERR_ACN_DECODE_MYPDU_A2"},
            {"name": "valid-shorter", "fields": (((0, 5), 1),),
             "success": True, "bits": 13, "error": "0",
             "value": {"a2.nCount": "1", "a2.arr[0]": "0xAF"}},
            {"name": "padding", "padding": True, "success": True, "bits": 37, "error": "0"},
        ),
        "value_fault": "decoded.a2.arr[0] ^= 1u;",
        "length_field": (0, 5),
    },
    "09-CHOICE/001.asn1#1": {
        "type": "ASN1SCC_MyPDU",
        "target_error": None,  # The constrained index read fails before the switch.
        "source_faults": {
            "missing-error-assignment": "*pErrCode = ret ? 0 : ERR_ACN_DECODE_MYPDU;",
        },
        "common": {"kind": "MyPDU_int1_PRESENT"},
        "seeds": (
            {"bits": 7, "wire": (0x14,), "assign": {"u.int1": "10"},
             "value": {"u.int1": "10"}},
            {"bits": 7, "wire": (0x16,), "assign": {"u.int1": "11"},
             "value": {"u.int1": "11"}},
        ),
        "case_name": "seed{seed}-{case}",
        # Replace just the three-bit selector; bit 7 is genuine padding.
        "cases": (
            {"name": "original", "success": True},
            {"name": "index5", "fields": (((0, 3), 5),),
             "success": False, "bits": 3, "error": "ERR_ACN_DECODE_MYPDU"},
            {"name": "index6", "fields": (((0, 3), 6),),
             "success": False, "bits": 3, "error": "ERR_ACN_DECODE_MYPDU"},
            {"name": "index7", "fields": (((0, 3), 7),),
             "success": False, "bits": 3, "error": "ERR_ACN_DECODE_MYPDU"},
            {"name": "padding", "padding": True, "success": True},
        ),
        "value_fault": "decoded.u.int1 ^= 1;",
    },
    "05-BOOLEAN/003.asn1#1": {
        "type": "ASN1SCC_MyPDU",
        "target_error": None,  # Pattern rejection adds branches, not statements.
        "source_faults": {
            "missing-error-assignment": "*pErrCode = ret ? 0 : ERR_ACN_DECODE_MYPDU;",
        },
        "common": {},
        # An empty field path denotes the scalar itself. Initialize each decode
        # to the opposite value so FALSE cannot pass through zero initialization.
        "seeds": (
            {"bits": 3, "wire": (0x20,), "assign": {"": "TRUE"},
             "value": {"": "TRUE"}, "initial": {"": "FALSE"}},
            {"bits": 3, "wire": (0x00,), "assign": {"": "FALSE"},
             "value": {"": "FALSE"}, "initial": {"": "TRUE"}},
        ),
        "case_name": "seed{seed}-{case}",
        "cases": (
            {"name": "original", "success": True},
            {"name": "pattern2", "fields": (((0, 3), 2),),
             "success": False, "bits": 3, "error": "ERR_ACN_DECODE_MYPDU"},
            {"name": "pattern7", "fields": (((0, 3), 7),),
             "success": False, "bits": 3, "error": "ERR_ACN_DECODE_MYPDU"},
            {"name": "padding", "padding": True, "success": True},
        ),
        "value_fault": "decoded = !decoded;",
        "value_faults": {
            "wrong-positive-value-true": "if (decoded == TRUE) decoded = FALSE;",
            "wrong-positive-value-false": "if (decoded == FALSE) decoded = TRUE;",
        },
    },
    "18-NULL/001.asn1#2": {
        "type": "ASN1SCC_MyPDU",
        "target_error": None,  # Pattern rejection adds branches, not statements.
        "source_faults": {
            "missing-rejection-assignment": "ret = ret && bDecodingPatternMatches;",
            "missing-error-assignment": "*pErrCode = ret ? 0 : ERR_ACN_DECODE_MYPDU;",
        },
        "common": {},
        # None explicitly means there is no distinguishable logical output.
        "seeds": (
            {"bits": 3, "wire": (0x20,), "assign": {"": "0"}, "value": None},
        ),
        "cases": (
            {"name": "original", "success": True},
            {"name": "pattern0", "fields": (((0, 3), 0),),
             "success": False, "bits": 3, "error": "ERR_ACN_DECODE_MYPDU"},
            {"name": "pattern1", "fields": (((0, 3), 7),),
             "success": False, "bits": 3, "error": "ERR_ACN_DECODE_MYPDU"},
            {"name": "padding", "padding": True, "success": True},
        ),
    },
}
SUPPORTED_UNITS = (*LAYOUTS, *STREAM_PROFILES)


def stream_cases(profile, seed_index):
    """Expand named operations without relying on case positions in the driver."""
    seed = profile["seeds"][seed_index]
    mutations = profile.get("cases")
    if mutations is None:
        mutations = [{"name": f"seed{seed_index}-original", "success": True}]
        mutations += [{"name": f"seed{seed_index}-" + "-".join(map(str, values)),
                       "fields": tuple(zip(profile["fields"], values)), "success": False}
                      for values in profile["invalid_fields"]]
        if seed["bits"] % 8:
            mutations.append({"name": f"seed{seed_index}-padding", "padding": True, "success": True})
    cases = []
    for mutation in mutations:
        fields, padding = mutation.get("fields", ()), mutation.get("padding", False)
        wire = int.from_bytes(bytes(seed["wire"]), "big")
        total_bits = len(seed["wire"]) * 8
        for (offset, width), value in fields:
            if not (0 <= offset and offset + width <= seed["bits"] and 0 <= value < 1 << width):
                raise ValueError("Unbounded profile field mutation")
            shift = total_bits - offset - width
            wire = (wire & ~(((1 << width) - 1) << shift)) | (value << shift)
        if padding:
            if seed["bits"] % 8 == 0:
                raise ValueError("Padding mutation requires actual padding")
            wire ^= 1
        success = mutation["success"]
        bits = mutation.get("bits", seed["bits"] if success else profile.get("rejection_bits"))
        if bits is None or not 0 <= bits <= total_bits:
            raise ValueError("Case consumption exceeds input view")
        name = profile.get("case_name", "{case}").format(seed=seed_index, case=mutation["name"])
        value = mutation.get("value", seed["value"])
        cases.append({"name": name, "fields": fields, "padding": padding,
                      "wire": wire.to_bytes(len(seed["wire"]), "big"), "success": success,
                      "bits": bits,
                      "initial": mutation.get("initial", seed.get("initial", {})),
                      "value": None if value is None else {**value, **profile["common"]},
                      "error": mutation.get("error", "0" if success else profile["target_error"])})
    return cases


def local_error_definitions(work, symbols):
    """Make private decoder errors available only to the optional main driver."""
    header = (work / "sample1.h").read_text()
    source = (work / "sample1.c").read_text()
    definitions = []
    for symbol in sorted(set(symbols) - {"0"}):
        pattern = r"^#define\s+" + re.escape(symbol) + r"\s+(\d+)\b"
        if re.search(pattern, header, re.M):
            continue
        values = re.findall(pattern, source, re.M)
        if len(values) != 1:
            raise ValueError("Missing or ambiguous private decoder error: " + symbol)
        definitions.append(f"#define {symbol} {values[0]}")
    return "\n".join(definitions)


STREAM_DRIVER = r'''
#include <stdio.h>
#include <string.h>
#include "sample1.h"
@ERRORS@

typedef struct { unsigned int offset, width, value; } CoverageField;
typedef struct {
    const char *name;
    const byte *wire;
    const CoverageField *fields;
    size_t field_count;
    int padding, success, error, bits, value_index;
} CoverageStreamCase;

static int coverage_profile_encode(const @TYPE@ *value, BitStream *stream, int *error)
{
    const int expected_error = @ENCODE_ERROR@;
    flag accepted = @TYPE@_ACN_Encode(value, stream, error, TRUE);
    return accepted && *error == expected_error;
}

static int coverage_profile_value(const @TYPE@ *value, int value_index)
{
    (void)value; /* Profiles without logical values have no predicate cases. */
    switch (value_index) {
@VALUES@
    default: return 0;
    }
}

static void coverage_profile_initialize(@TYPE@ *value, int value_index)
{
    memset(value, 0, sizeof *value);
    switch (value_index) {
@INITIAL_VALUES@
    default: break;
    }
}

static void coverage_profile_field(byte *input, CoverageField field)
{
    for (unsigned int bit = 0; bit < field.width; ++bit) {
        unsigned int position = field.offset + bit;
        byte mask = (byte)(1u << (7u - position % 8u));
        input[position / 8u] = (byte)((input[position / 8u] & (byte)~mask)
            | (((field.value >> (field.width - bit - 1u)) & 1u) ? mask : 0u));
    }
}

static int coverage_profile_cases(const byte *encoded, size_t size,
                                  const CoverageStreamCase *cases, size_t case_count)
{
    for (size_t case_index = 0; case_index < case_count; ++case_index) {
        const CoverageStreamCase *test = &cases[case_index];
        /* Exact-size stack views let ASan observe reads/writes beyond the wire. */
        byte input[size], saved[size];
        memcpy(input, encoded, size);
        for (size_t field_index = 0; field_index < test->field_count; ++field_index) {
            coverage_profile_field(input, test->fields[field_index]);
        }
        if (test->padding) input[size - 1] ^= 1u;
        if (memcmp(input, test->wire, size) != 0) {
            puts("Stream profile mutation: unexpected field/padding bytes");
            return 1;
        }
        memcpy(saved, input, size);
        @TYPE@ decoded;
        coverage_profile_initialize(&decoded, test->value_index);
        if (test->success && test->value_index >= 0
            && coverage_profile_value(&decoded, test->value_index)) {
            puts("Stream profile: unexpected initial positive value");
            return 1;
        }
        BitStream stream;
        int error = 0;
        BitStream_AttachBuffer(&stream, input, size);
        flag accepted = @TYPE@_ACN_Decode(&decoded, &stream, &error);
        if (accepted != test->success || error != test->error
            || stream.currentByte * 8 + stream.currentBit != test->bits
            || stream.count != (long)size || memcmp(input, saved, size) != 0
            || (accepted && test->value_index >= 0
                && !coverage_profile_value(&decoded, test->value_index))) {
            printf("Stream profile @UNIT@/%s: unexpected result/error/value/stream\n", test->name);
            return 1;
        }
        printf("Stream profile @UNIT@/%s: expected %s, OK\n", test->name,
               test->success ? "success" : "rejection");
    }
    return 0;
}

static int coverage_invalid_stream_checks(void)
{
@SEEDS@
    puts("Stream profile checks (@COUNT@) run successfully.");
    return 0;
}
'''


def value_access(base, field, pointer=False):
    """Address either a structured member or the scalar at an empty field path."""
    if not field:
        return f"(*{base})" if pointer else base
    return base + ("->" if pointer else ".") + field


def stream_driver(work, unit, mode="acn"):
    profile = STREAM_PROFILES[unit]
    typ = profile["type"]
    encode_error = profile.get("encode_errors", {}).get(mode, "0")
    values, initial_values, blocks, all_cases = [], [], [], []
    c_bytes = lambda wire: "{" + ", ".join(hex(b) for b in wire) + "}"
    for index, seed in enumerate(profile["seeds"]):
        size = len(seed["wire"])
        if size != (seed["bits"] + 7) // 8:
            raise ValueError("Seed wire size does not match bit length")
        assignments = "\n".join(f"        {value_access('value', field)} = {value};"
                                for field, value in {**profile["common"], **seed["assign"]}.items())
        cases = stream_cases(profile, index)
        all_cases.extend(cases)
        declarations, rows = [], []
        for number, case in enumerate(cases):
            value_index = -1  # No logical-value assertions (e.g. NULL).
            if case["value"] is not None:
                if not case["value"]:
                    raise ValueError("Value checks require fields; use None when inapplicable")
                value_index = len(values)
                predicate = " && ".join(f"{value_access('value', field, pointer=True)} == {value}"
                                        for field, value in case["value"].items())
                values.append(f"    case {value_index}: return {predicate};")
            if case["initial"]:
                if value_index < 0:
                    raise ValueError("Initial logical values require an applicable value check")
                initial = " ".join(f"{value_access('value', field, pointer=True)} = {value};"
                                   for field, value in case["initial"].items())
                initial_values.append(f"    case {value_index}: {initial} break;")
            declarations.append(f"        static const byte wire_{number}[] = {c_bytes(case['wire'])};")
            ops = ", ".join("{%d, %d, %d}" % (*field, value) for field, value in case["fields"])
            if ops:
                declarations.append(f"        static const CoverageField fields_{number}[] = {{{ops}}};")
            rows.append(f"            {{{json.dumps(case['name'])}, wire_{number}, "
                        + (f"fields_{number}" if ops else "NULL")
                        + f", {len(case['fields'])}, {int(case['padding'])}, {int(case['success'])}, {case['error']}, {case['bits']}, {value_index}}}")
        blocks.append(f'''    {{
        {typ} value;
        memset(&value, 0, sizeof value);
{assignments}
        byte encoded[{size}] = {{0}};
        const byte canonical[{size}] = {c_bytes(seed['wire'])};
        BitStream stream;
        int error = 0;
        BitStream_Init(&stream, encoded, sizeof encoded);
        if (!coverage_profile_encode(&value, &stream, &error)
            || stream.currentByte * 8 + stream.currentBit != {seed['bits']}
            || stream.count != {size} || BitStream_GetLength(&stream) != {size}
            || memcmp(encoded, canonical, sizeof encoded) != 0) {{
            puts("Stream profile seed: unexpected encoding/layout");
            return 1;
        }}
{chr(10).join(declarations)}
        static const CoverageStreamCase cases[] = {{
{(',' + chr(10)).join(rows)}
        }};
        if (coverage_profile_cases(encoded, sizeof encoded, cases,
                                   sizeof cases / sizeof cases[0])) return 1;
    }}''')
    driver = STREAM_DRIVER
    replacements = {"UNIT": unit, "TYPE": typ, "VALUES": "\n".join(values),
                    "INITIAL_VALUES": "\n".join(initial_values),
                    "SEEDS": "\n".join(blocks), "COUNT": str(len(all_cases)),
                    "ENCODE_ERROR": encode_error,
                    "ERRORS": local_error_definitions(work, [case["error"] for case in all_cases])}
    for key, value in replacements.items():
        driver = driver.replace("@" + key + "@", value)
    return driver, {"profile_unit": unit, "case_ids": [case["name"] for case in all_cases],
                    "success": [case["success"] for case in all_cases],
                    "encode_calls": len(profile["seeds"]), "decode_calls": len(all_cases),
                    "negative_decodes": sum(not case["success"] for case in all_cases),
                    "positive_decodes": sum(case["success"] for case in all_cases)}



# Positive API obligations are measured before and alongside stream mutations.
# Parameterized types can have an initializer but no standalone codec/ATC.
POSITIVE_INITIALIZERS = {
    "15-PUS-ParameterPassing/001.asn1#1": "ASN1SCC_COLOR_DATA",
}


def initializer_target_lines(work, unit):
    typ = POSITIVE_INITIALIZERS.get(unit)
    if typ is None:
        return []
    lines = (work / "sample1.c").read_text().splitlines()
    starts = [i for i, line in enumerate(lines) if line.startswith(f"void {typ}_Initialize(")]
    if len(starts) != 1:
        raise ValueError("Missing or ambiguous positive initializer")
    start = starts[0]
    end = next(i for i in range(start + 1, len(lines)) if lines[i] == "}")
    targets = [i + 1 for i in range(start, end)
               if lines[i].strip() == "(void)pVal;"
               or lines[i].strip() == f"(*(pVal)) = ({typ}){typ}_constant;"]
    if len(targets) != 2:
        raise ValueError("Unexpected initializer body")
    return targets


def positive_initializer_driver(unit):
    if unit not in POSITIVE_INITIALIZERS:
        return ""
    return r'''#include <stdio.h>
#include <string.h>
#include "sample1.h"

static int coverage_positive_initializer_checks(void)
{
    ASN1SCC_COLOR_DATA initialized;
    memset(&initialized, 0, sizeof initialized);
    initialized.kind = COLOR_DATA_red_PRESENT;
    initialized.u.red = 42;
    ASN1SCC_COLOR_DATA_Initialize(&initialized);
    int initializer_error = 0;
    if (initialized.kind != COLOR_DATA_green_PRESENT || initialized.u.green != 1
        || !ASN1SCC_COLOR_DATA_IsConstraintValid(&initialized, &initializer_error)
        || initializer_error != 0) {
        puts("Positive initializer: unexpected value/validation result");
        return 1;
    }
    puts("Positive initializer ASN1SCC_COLOR_DATA: OK");
    return 0;
}
'''


def prepare_positive_controls(work, unit, args):
    if unit not in POSITIVE_INITIALIZERS or args.language != "c" or args.encodings != "acn":
        raise ValueError("No explicit positive initializer control for this unit")
    path = work / "mainprogram.c"
    text = path.read_text()
    old = "return asn1scc_run_generated_testsuite(&output);"
    if text.count(old) != 1:
        raise ValueError("Unexpected generated test runner")
    driver = positive_initializer_driver(unit)
    replacement = ("int positives = asn1scc_run_generated_testsuite(&output);\n"
                   "    if (positives != 0) return positives;\n"
                   "    return coverage_positive_initializer_checks();")
    path.write_text(driver + "\n" + text.replace(old, replacement))
    return {"positive_only": True, "positive_initializers": [POSITIVE_INITIALIZERS[unit]],
            "initializer_target_lines": initializer_target_lines(work, unit),
            "driver_sha256": hashlib.sha256(driver.encode()).hexdigest()}


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


def target_lines(work, unit=UNIT):
    lines = (work / "sample1.c").read_text().splitlines()
    if unit in STREAM_PROFILES:
        error = STREAM_PROFILES[unit]["target_error"]
        if not error:
            return []
        matches = [i for i, line in enumerate(lines) if line.strip().startswith(f"*pErrCode = {error};")]
        if len(matches) != 1 or not lines[matches[0] + 1].strip().startswith("ret = FALSE;"):
            raise ValueError("Expected unique present-when error/rejection statements")
        return [matches[0] + 1, matches[0] + 2]
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


def source_fault_lines(work, unit):
    """Map semantic fault names even when generated assignment order differs."""
    lines = (work / "sample1.c").read_text().splitlines()
    result = {}
    for name, token in STREAM_PROFILES.get(unit, {}).get("source_faults", {}).items():
        matches = [i + 1 for i, line in enumerate(lines) if line.strip() == token]
        if len(matches) != 1:
            raise ValueError("Missing or ambiguous source fault assignment: " + name)
        result[name] = matches[0]
    for line in target_lines(work, unit):
        text = lines[line - 1].strip()
        if text.startswith("*pErrCode = "):
            result["missing-error-assignment"] = line
        elif text.startswith("ret = FALSE;"):
            result["missing-rejection-assignment"] = line
        else:
            raise ValueError("Unexpected target assignment")
    if unit in POSITIVE_INITIALIZERS:
        result["missing-initializer-assignment"] = initializer_target_lines(work, unit)[-1]
    return result


def prepare(work, unit, args):
    if unit not in SUPPORTED_UNITS or args.language != "c" or args.encodings != "acn":
        raise ValueError("No explicit invalid-stream oracle for this unit/language/encoding")
    targets = target_lines(work, unit)
    path = work / "mainprogram.c"
    text = path.read_text()
    old = "return asn1scc_run_generated_testsuite(&output);"
    if text.count(old) != 1:
        raise ValueError("Unexpected generated test runner")
    replacement = "int positives = asn1scc_run_generated_testsuite(&output);\n    if (positives != 0) return positives;\n    return coverage_invalid_stream_checks();"
    if unit in STREAM_PROFILES:
        driver, checks = stream_driver(work, unit, "acn-v2" if args.acn_v2 else "acn")
        if unit in POSITIVE_INITIALIZERS:
            driver = positive_initializer_driver(unit) + "\n" + driver
            replacement = replacement.replace("    return coverage_invalid_stream_checks();",
                "    if (coverage_positive_initializer_checks()) return 1;\n    return coverage_invalid_stream_checks();")
            checks.update(positive_initializers=[POSITIVE_INITIALIZERS[unit]],
                          initializer_target_lines=initializer_target_lines(work, unit))
        path.write_text(driver + "\n" + text.replace(old, replacement))
        return {**checks, "target_lines": targets,
                "driver_sha256": hashlib.sha256(driver.encode()).hexdigest()}
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
    for typ in checks.get("positive_initializers", []):
        if output.splitlines().count(f"Positive initializer {typ}: OK") != 1:
            raise ValueError("Missing or duplicate positive initializer control")
    if checks.get("positive_only"):
        if not checks.get("positive_initializers"):
            raise ValueError("Empty positive initializer control")
        return
    if "profile_unit" in checks:
        unit = checks["profile_unit"]
        profile = STREAM_PROFILES[unit]
        cases = [case for index in range(len(profile["seeds"])) for case in stream_cases(profile, index)]
        expected_ids = [case["name"] for case in cases]
        success = [case["success"] for case in cases]
        if (checks["case_ids"] != expected_ids or checks["success"] != success
                or checks["encode_calls"] != len(profile["seeds"])
                or checks["decode_calls"] != len(cases)
                or checks["negative_decodes"] != success.count(False)
                or checks["positive_decodes"] != success.count(True)):
            raise ValueError("Inconsistent stream profile accounting")
        expected = [f"Stream profile {unit}/{case['name']}: expected "
                    + ("success" if case["success"] else "rejection") + ", OK" for case in cases]
        actual = [line for line in output.splitlines() if line.startswith("Stream profile ")
                  and not line.startswith("Stream profile checks (")]
        if actual != expected:
            raise ValueError("Missing, duplicate or unexpected stream profile outcome")
        if output.splitlines().count(f"Stream profile checks ({len(cases)}) run successfully.") != 1:
            raise ValueError("Missing stream profile summary")
        return
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
