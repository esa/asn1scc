"""Explicit, representable invalid C values; encoders always check constraints."""
import hashlib
import re


CASES = {
    "09-CHOICE/003.asn1#3": {
        "type": "MyPDU", "validator": "ChoiceType", "target_error": "ERR_CHOICETYPE",
        "values": [
            {"name": "unset-choice", "error": "ERR_CHOICETYPE",
             "assign": "bad.a = ChoiceSelector_int1; bad.b.kind = ChoiceType_NONE;"},
            {"name": "out-of-range-child", "error": "ERR_CHOICETYPE_INT1",
             "assign": "bad.a = ChoiceSelector_int1; bad.b.kind = ChoiceType_int1_PRESENT; bad.b.u.int1 = 16;"},
        ],
    },
    "09-CHOICE/011.asn1#1": {
        "type": "T_SubThing", "validator": "T_Thing", "target_error": "ERR_T_THING",
        "values": [
            {"name": "unset-choice", "error": "ERR_T_THING", "assign": "bad.kind = T_Thing_NONE;"},
            {"name": "forbidden-alternative", "error": "ERR_T_SUBTHING_2",
             "assign": "bad.kind = T_Thing_field1_PRESENT; bad.u.field1 = 0;"},
        ],
    },
}
OPERATIONS = ("validate", "uper", "acn")


def driver(case):
    body = r'''
#include <stdio.h>
#include <string.h>
#include "sample1.h"

static int coverage_invalid_value_checks(void)
{
    static const char *names[] = {NAMES};
    static const int errors[] = {ERRORS};
    static const char *operations[] = {"uper", "acn"};
    for (int i = 0; i < 2; ++i) {
        TYPE bad;
        memset(&bad, 0, sizeof bad);
        if (i == 0) {
            FIRST_VALUE
        } else {
            SECOND_VALUE
        }
        int error = 0;
        flag accepted = TYPE_IsConstraintValid(&bad, &error);
        if (accepted || error != errors[i]) {
            printf("Invalid value %s/validate: unexpected %d/%d\n", names[i], accepted, error);
            return 1;
        }
        printf("Invalid value %s/validate: expected rejection, OK\n", names[i]);
        for (int encoding = 0; encoding < 2; ++encoding) {
            /* Capacity covers both encodings, including zero-sized types. */
            byte buffer[TYPE_REQUIRED_BYTES_FOR_ENCODING + TYPE_REQUIRED_BYTES_FOR_ACN_ENCODING + 1];
            byte saved[sizeof buffer];
            BitStream stream;
            BitStream_Init(&stream, buffer, (long)sizeof buffer);
            memset(buffer, 0xA5, sizeof buffer);
            memcpy(saved, buffer, sizeof buffer);
            error = 0;
            accepted = encoding == 0
                ? TYPE_Encode(&bad, &stream, &error, TRUE)
                : TYPE_ACN_Encode(&bad, &stream, &error, TRUE);
            if (accepted || error != errors[i] || stream.currentByte != 0 || stream.currentBit != 0
                || stream.count != (long)sizeof buffer || memcmp(buffer, saved, sizeof buffer) != 0) {
                printf("Invalid value %s/%s: unexpected result/error or modified stream\n", names[i], operations[encoding]);
                return 1;
            }
            printf("Invalid value %s/%s: expected rejection, OK\n", names[i], operations[encoding]);
        }
    }
    puts("Invalid value checks (6) run successfully.");
    return 0;
}
'''
    replacements = {
        "NAMES": ", ".join('"' + v["name"] + '"' for v in case["values"]),
        "ERRORS": ", ".join(v["error"] for v in case["values"]),
        "FIRST_VALUE": case["values"][0]["assign"],
        "SECOND_VALUE": case["values"][1]["assign"],
        "TYPE": "ASN1SCC_" + case["type"],
    }
    # Expand once: replacement text such as ERR_CHOICETYPE must not itself
    # be interpreted as containing the TYPE placeholder.
    return re.sub("|".join(map(re.escape, replacements)), lambda match: replacements[match[0]], body)


def target_lines(work, case):
    """Select both default-rejection statements inside the intended validator."""
    lines = (work / "sample1.c").read_text().splitlines()
    signature = "flag ASN1SCC_" + case["validator"] + "_IsConstraintValid("
    starts = [i for i, line in enumerate(lines) if line.startswith(signature)]
    if len(starts) != 1:
        raise ValueError("Missing or ambiguous target validator")
    start = starts[0]
    stop = next((i for i in range(start + 1, len(lines)) if lines[i] == "}"), None)
    if stop is None:
        raise ValueError("Unterminated target validator")
    needle = "*pErrCode = " + case["target_error"] + ";"
    matches = [i for i in range(start, stop) if lines[i].strip().startswith(needle)]
    if len(matches) != 1 or not lines[matches[0] + 1].strip().startswith("ret = FALSE;"):
        raise ValueError("Expected two explicit default-rejection statements")
    return [matches[0] + 1, matches[0] + 2]


def prepare(work, unit, args):
    case = CASES.get(unit)
    if case is None or args.encodings != "both" or args.language != "c":
        raise ValueError("No explicit invalid-value oracle for this unit/language/encoding")
    targets = target_lines(work, case)
    path = work / "mainprogram.c"
    original = path.read_text()
    old = "return asn1scc_run_generated_testsuite(&output);"
    if original.count(old) != 1:
        raise ValueError("Unexpected generated test runner")
    replacement = "int positives = asn1scc_run_generated_testsuite(&output);\n    if (positives != 0) return positives;\n    return coverage_invalid_value_checks();"
    source = driver(case)
    path.write_text(source + "\n" + original.replace(old, replacement))
    return {"values": 2, "api_checks": 6, "case": case, "target_lines": targets,
            "driver_sha256": hashlib.sha256(source.encode()).hexdigest()}


def verify_output(output, checks):
    positive = re.search(r"All test cases \((\d+)\) run successfully", output)
    if not positive or int(positive[1]) == 0:
        raise ValueError("Missing original positive tests")
    if output.count("Invalid value checks (6) run successfully.") != 1:
        raise ValueError("Missing invalid-value checks")
    for value in checks["case"]["values"]:
        for operation in OPERATIONS:
            expected = f"Invalid value {value['name']}/{operation}: expected rejection, OK"
            if output.count(expected) != 1:
                raise ValueError("Missing or unexpected invalid-value outcome: " + expected)


# Optional goals compose with the stream driver, independently of CASES and the
# collector's C/both-only invalid-value option. Registry order is report order.
EXTRA_PROFILES = {
    "choice-unset": {
        "unit": "09-CHOICE/001.asn1#1", "type": "MyPDU",
        "validator": "MyPDU", "member": "", "kind": "MyPDU_NONE",
        "target_error": "ERR_MYPDU",
    },
    "payload-unset": {
        "unit": "09-CHOICE/013.asn1#1", "type": "MyPDU",
        "validator": "MyPayload", "member": "payload", "kind": "MyPayload_NONE",
        "target_error": "ERR_MYPAYLOAD",
    },
    "color-unset": {
        "unit": "15-PUS-ParameterPassing/001.asn1#1", "type": "MySeq",
        "validator": "COLOR_DATA", "member": "colorData", "kind": "COLOR_DATA_NONE",
        "target_error": "ERR_COLOR_DATA",
    },
}
EXTRA_GOALS = {goal: profile["unit"] for goal, profile in EXTRA_PROFILES.items()}
EXTRA_OPERATIONS = ("validate", "validate-parent", "encode")


def extra_driver(goal, profile):
    """Each operation starts with a fresh initialized parent and error state."""
    parent = "ASN1SCC_" + profile["type"]
    child = "bad" + ("." + profile["member"] if profile["member"] else "")
    blocks = []
    for operation in EXTRA_OPERATIONS:
        call = (f"ASN1SCC_{profile['validator']}_IsConstraintValid(&{child}, &error)"
                if operation == "validate" else f"{parent}_IsConstraintValid(&bad, &error)")
        setup = ""
        preservation = ""
        if operation == "encode":
            setup = f"""
        byte buffer[{parent}_REQUIRED_BYTES_FOR_ACN_ENCODING + 1];
        byte saved[sizeof buffer];
        BitStream stream;
        memset(buffer, 0xA5, sizeof buffer);
        memcpy(saved, buffer, sizeof buffer);
        BitStream_AttachBuffer(&stream, buffer, (long)sizeof buffer);
"""
            call = f"{parent}_ACN_Encode(&bad, &stream, &error, TRUE)"
            preservation = f"""
        if (stream.currentByte != 0 || stream.currentBit != 0
            || stream.count != (long)sizeof buffer || memcmp(buffer, saved, sizeof buffer)) {{
            puts("Value goal {goal}/{operation}: unexpected stream change");
            return 1;
        }}
"""
        blocks.append(f"""
    /* begin {goal}/{operation} */
    {{
        {parent} bad;
        memset(&bad, 0, sizeof bad);
        {parent}_Initialize(&bad);
        int error = 0;
        if (!{parent}_IsConstraintValid(&bad, &error) || error != 0) {{
            puts("Value goal {goal}/{operation}: unexpected initial value");
            return 1;
        }}
        {child}.kind = {profile['kind']};
        error = 0;
{setup}
        flag accepted = {call};
        /* observe {goal}/{operation} */
        if (accepted != FALSE) {{
            puts("Value goal {goal}/{operation}: unexpected result");
            return 1;
        }}
        if (error != {profile['target_error']}) {{
            puts("Value goal {goal}/{operation}: unexpected error");
            return 1;
        }}
{preservation}
        puts("Value goal {goal}/{operation}: OK");
    }}
    /* end {goal}/{operation} */
""")
    return "\n".join(blocks)


def prepare_extra(work, unit, args):
    goals = [goal for goal, selected in EXTRA_GOALS.items() if selected == unit]
    metadata = {"goal_ids": goals, "operations": {}, "target_lines": {}}
    if not goals:
        return metadata
    if args.language != "c" or args.encodings != "acn":
        raise ValueError("Extra value goals require C/ACN")
    path = work / "mainprogram.c"
    original = path.read_text()
    signature = "int main(int argc, char* argv[])"
    if original.count(signature) != 1 or "coverage_extra_value_checks" in original:
        raise ValueError("Unexpected or already composed test runner")
    bodies = []
    for goal in goals:
        profile = EXTRA_PROFILES[goal]
        metadata["operations"][goal] = list(EXTRA_OPERATIONS)
        metadata["target_lines"][goal] = target_lines(work, profile)
        bodies.append(extra_driver(goal, profile))
    source = ('#include <stdio.h>\n#include <string.h>\n#include "sample1.h"\n'
              + "static int coverage_extra_value_checks(void) {\n"
              + "\n".join(bodies) + "\nreturn 0;\n}\n")
    path.write_text(source + original.replace(signature, "static int coverage_previous_main(int argc, char* argv[])")
                    + "\nint main(int argc, char* argv[]) {\n"
                    + "    int result = coverage_previous_main(argc, argv);\n"
                    + "    if (result != 0) return result;\n"
                    + "    return coverage_extra_value_checks();\n}\n")
    metadata["driver_sha256"] = hashlib.sha256(source.encode()).hexdigest()
    metadata["api_checks"] = sum(len(ops) for ops in metadata["operations"].values())
    return metadata


def verify_extra_output(output, metadata):
    expected = [f"Value goal {goal}/{operation}: OK"
                for goal in metadata["goal_ids"] for operation in EXTRA_OPERATIONS]
    actual = [line for line in output.splitlines() if line.startswith("Value goal ")]
    if sorted(actual) != sorted(expected):
        raise ValueError("Incomplete, duplicate or unexpected extra value outcomes")
