#!/usr/bin/env python3
"""Classify the uncovered statement obligations of statementCollector.py runs.

Reads one or more exported measurement directories (each with units.jsonl and
units/<id>/work/ holding the generated sources), finds the generated function
that encloses every codec statement obligation and groups the obligations by
function kind and statement kind. The labels are source patterns only: they
say where a missed statement is, not whether a test can reach it.

The one exception is the `proven` column: a missed statement gets the ID of a
documented statement-coverage exemption (Docs/statement-coverage-exemptions.md)
when the source around it satisfies that exemption's precondition, checked
here statement by statement. The report then gives raw coverage, the proven
unreachable statements and the remaining unproven misses.
"""
import argparse
from collections import Counter, defaultdict
import csv
import json
from pathlib import Path
import re
import sys

C_FUNCTION = re.compile(r"^[A-Za-z_][\w \*]*?\b(\w+)\s*\([^;]*$")
ADA_FUNCTION = re.compile(r"^\s*(?:overriding\s+)?(?:function|procedure)\s+(\w+)", re.I)
ERROR_TEXT = re.compile(r"\bERR_\w+|Success\s*=>\s*False|\bret\s*(=|:=)\s*(FALSE|False)\b"
                        r"|\breturn\s+(FALSE|False)\b|\.Success\s*:=\s*False")


def function_kind(name):
    """Kind of a generated function, from the asn1scc naming scheme."""
    fn = name.lower()
    if "isconstraintvalid" in fn or "charsarevalid" in fn:
        return "validate"
    if fn.endswith("_equal") or "_equal_" in fn:
        return "equal"
    if "_initialize" in fn or "_constant" in fn or fn.endswith("_init") or "_init_" in fn:
        return "init"
    direction = "encode" if "_enc" in fn else ("decode" if "_dec" in fn else None)
    if direction and "_acn" in fn:
        return "acn-" + direction
    if direction and ("_xer_" in fn or fn.endswith("_xer")):
        return "xer-" + direction
    if direction and ("_ber_" in fn or fn.endswith("_ber")):
        return "ber-" + direction
    if direction:
        return "uper-" + direction
    return "other"


DEFAULT_ARM = re.compile(r"^\s*(default\s*:|when\s+others\s*=>)", re.I)


def in_default_arm(lines, first):
    """True when the statement belongs to a switch 'default:' / Ada 'when others' arm."""
    for number in range(first, max(first - 4, 0), -1):
        text = lines[number - 1]
        if DEFAULT_ARM.match(text):
            return True
        if re.match(r"^\s*(case\b|when\b|\}|break;)", text) and number != first:
            return False
    return False


def statement_kind(src, lines, first, last):
    """Kind of a statement. The source lines (not only the statement text
    GNATcoverage reports) decide COVERAGE_IGNORE, which is a trailing comment.
      error:   reports a failure (error code, FALSE result);
      default: any statement of a switch default / 'when others' arm;
      other:   everything else.
    '-ignored' is appended when the lines carry COVERAGE_IGNORE."""
    full = " ".join(lines[first - 1:last])
    kind = "default" if in_default_arm(lines, first) else (
        "error" if ERROR_TEXT.search(src) else "other")
    return kind + ("-ignored" if "COVERAGE_IGNORE" in full else "")


# The Int8 / UInt16 / ... variants (slim integer types) only cast the result of the two
# base functions (asn1crt/asn1crt_encoding.c), so the same proof holds.
RTL_DECODE = (r"BitStream_DecodeConstraint(?:Pos)?WholeNumber(?:U?Int(?:8|16|32))?\(pBitStrm,\s*\(?&\(?{var}\)?\)?,"
              r"\s*(-?\d+),\s*(-?\d+)\)")
CHAR_GUARD = re.compile(r"\bif\s*\(ret\s*&&\s*\(charIndex\s*<\s*0\s*\|\|\s*charIndex\s*>\s*(\d+)\)\)\s*\{")


def code_only(text):
    """A C line without comments and string/char literals, for brace counting."""
    text = re.sub(r"/\*.*?\*/|//.*$", "", text)
    return re.sub(r'"(\\.|[^"\\])*"|' + r"'(\\.|[^'\\])*'", '""', text)


def decoded_range(lines, before, var, window=4):
    """(min, max) when one of the `window` lines before line `before` (1-based)
    calls BitStream_DecodeConstraint[Pos]WholeNumber(pBitStrm, &var, min, max)."""
    pattern = re.compile(RTL_DECODE.format(var=re.escape(var)))
    assigned = re.compile(r"(?<![\w.>])" + re.escape(var) + r"\s*(=(?!=)|\+\+|--|[-+*/%&|^]=)")
    for number in range(before - 1, max(before - 1 - window, 0), -1):
        match = pattern.search(lines[number - 1])
        if match:
            return int(match[1]), int(match[2])
        if assigned.search(code_only(lines[number - 1])):
            return None  # the selector changes between the decode and its use
    return None


FIXED_SIZE_DECODE = (r"Acn_Dec_Int_PositiveInteger_ConstSize(?:_(8)|_(?:big|little)_endian_(16|32|64))?"
                     r"\(pBitStrm,\s*\(?&\(?{var}\)?\)?(?:,\s*(\d+))?\)")


def fixed_size_range(lines, before, var, window=4):
    """(0, 2**N - 1) when one of the `window` lines before line `before` decodes
    `var` with an N-bit unsigned fixed-size ACN decoder."""
    pattern = re.compile(FIXED_SIZE_DECODE.format(var=re.escape(var)))
    assigned = re.compile(r"(?<![\w.>])" + re.escape(var) + r"\s*(=(?!=)|\+\+|--|[-+*/%&|^]=)")
    for number in range(before - 1, max(before - 1 - window, 0), -1):
        match = pattern.search(lines[number - 1])
        if match:
            bits = match[1] or match[2] or match[3]
            return (0, 2 ** int(bits) - 1) if bits else None
        if assigned.search(code_only(lines[number - 1])):
            return None  # the selector changes between the decode and its use
    return None


def covers(labels, bounds):
    """True when the sorted labels are exactly bounds[0] .. bounds[1] (compared by
    count first: a 64-bit range must not be expanded)."""
    return len(labels) == bounds[1] - bounds[0] + 1 and labels == list(range(bounds[0], bounds[1] + 1))


def default_arm_switch(lines, first):
    """(selector, switch head line, case labels) of the switch whose `default:`
    arm holds the statement, or None. The labels are those of that switch
    only; None when one of them is not an integer literal."""
    default = None
    for number in range(first, max(first - 4, 0), -1):
        if re.match(r"^\s*default\s*:", lines[number - 1]):
            default = number
            break
        if re.match(r"^\s*(case\b|\}|break;)", lines[number - 1]) and number != first:
            return None
    if default is None:
        return None
    # The switch that owns the arm: walk up to the unmatched '{' of the block
    # holding the `default:` line; the head is that line or the one before it.
    head, depth = default - 1, 0
    while head > 0:
        text = code_only(lines[head - 1])
        depth += text.count("}") - text.count("{")
        if depth < 0:
            break
        head -= 1
    if head > 1 and code_only(lines[head - 1]).strip() == "{":
        head -= 1
    match = re.match(r"^\s*switch\s*\(\s*(\w+)\s*\)\s*\{?\s*$", lines[head - 1]) if head else None
    if not match:
        return None
    depth, labels = 0, []
    for number in range(head, default):
        text = code_only(lines[number - 1])
        if depth == 1:
            if re.search(r"\bcase\b(?!\s+-?\d+\s*:)", text):
                return None  # a non-numeric label: not the decoded-value shape
            labels += [int(v) for v in re.findall(r"\bcase\s+(-?\d+)\s*:", text)]
        depth += text.count("{") - text.count("}")
        if depth <= 0 and number > head:
            return None  # the switch closed before the default arm
    return (match[1], head, sorted(labels)) if depth == 1 else None


def cov001(lines, first):
    """ASN1SCC-COV-001: the statement is in the `default:` arm of a switch whose
    selector was just decoded by BitStream_DecodeConstraint[Pos]WholeNumber(.., min, max)
    and whose own case labels are exactly min..max."""
    arm = default_arm_switch(lines, first)
    if arm is None:
        return False
    var, head, labels = arm
    bounds = decoded_range(lines, head, var)
    return bounds is not None and covers(labels, bounds)


def cov003(lines, first):
    """ASN1SCC-COV-003: the statement is in the `default:` arm of a switch whose
    selector was just decoded by an N-bit unsigned fixed-size ACN decoder and
    whose own case labels are exactly 0 .. 2**N - 1."""
    arm = default_arm_switch(lines, first)
    if arm is None:
        return False
    var, head, labels = arm
    bounds = fixed_size_range(lines, head, var)
    return bounds is not None and covers(labels, bounds)


def cov002(lines, first, src):
    """ASN1SCC-COV-002: the body of the charIndex guard, right after
    BitStream_DecodeConstraintWholeNumber(.., &charIndex, 0, N) with the same N."""
    match = CHAR_GUARD.search(lines[first - 1])
    if not match or src.lstrip().startswith("if"):
        return False
    return decoded_range(lines, first, "charIndex") == (0, int(match[1]))


# ASN1SCC-COV-004 / -005: the generated encoders are structured C (blocks, if / else,
# switch, one statement per line), which the helpers below follow line by line.

def c_function_head(lines, line):
    """Line number of the definition of the C function that holds `line`, or None."""
    for number in range(line, 0, -1):
        text = lines[number - 1]
        if not text.strip() or text[:1].isspace() or text.startswith(("#", "/", "{")):
            continue
        if text.startswith("}"):
            return None
        match = C_FUNCTION.match(text)
        if match and match[1] not in {"if", "while", "for", "switch", "return", "sizeof"}:
            return number
        return None
    return None


def block_stacks(lines, start, end):
    """{line: enclosing blocks} for the lines start..end (1-based) of a C function,
    each block a tuple (open line, header, current case label). The header is the
    code before the block's '{' (or the line before it when the '{' stands alone);
    the case label is the last `case L:` met directly in a switch block."""
    stacks, stack, previous = {}, [], ""
    for number in range(start, end + 1):
        text = code_only(lines[number - 1])
        stacks[number] = tuple(stack)
        label = re.match(r"^\s*(case\s+([^:]+?)\s*:|default\s*:)", text)
        if label and stack and re.match(r"^\s*switch\b", stack[-1][1]):
            stack[-1] = (stack[-1][0], stack[-1][1], label[2].strip() if label[2] else "default")
        pos = 0
        for match in re.finditer(r"[{}]", text):
            if match[0] == "{":
                header = text[pos:match.start()].strip()
                header = re.sub(r"^\}\s*", "", header)
                if not header and not previous.endswith(("{", "}", ";", ":")):
                    header = previous  # `switch (E)` / a function head on the line before the '{'
                stack.append((number, header, None))
            elif stack:
                stack.pop()
            pos = match.end()
        if text.strip() and text.strip() != "{":
            previous = text.strip()
    return stacks


def norm(expr):
    expr = re.sub(r"\s+", "", expr)
    while expr.startswith("(") and expr.endswith(")") and balanced(expr[1:-1]):
        expr = expr[1:-1]
    return expr


def balanced(text):
    depth = 0
    for char in text:
        depth += {"(": 1, ")": -1}.get(char, 0)
        if depth < 0:
            return False
    return depth == 0


def guard(block):
    """The condition under which a block runs: ('ret',), ('if', C), ('elif', C) (C and the
    earlier conditions of the chain false), ('case', E, L), None for a plain block,
    ('other',) for else / loops."""
    _, header, label = block
    match = re.match(r"^if\s*\((.*)\)$", header)
    if match:
        cond = norm(match[1])
        return ("ret",) if cond == "ret" or cond.startswith("ret&&") else ("if", cond)
    match = re.match(r"^else\s+if\s*\((.*)\)$", header)
    if match:
        return ("elif", norm(match[1]))  # runs when the condition holds, but not only then
    match = re.match(r"^switch\s*\((.*)\)$", header)
    if match:
        return ("case", norm(match[1]), label)
    if header == "" or C_FUNCTION.match(header) or re.match(r"^[\w ]+\*?\s*\w+\s*\(.*\)$", header):
        return None
    return ("other",)


def same_guard(a, b):
    """True when guards a and b are the same condition: also `if (E == L)` against
    `case L:` of `switch (E)`."""
    if a == b:
        return True
    for x, y in ((a, b), (b, a)):
        if x[0] == "if" and y[0] == "case" and y[2] not in (None, "default"):
            if x[1] in (f"{y[1]}=={norm(y[2])}", f"{norm(y[2])}=={y[1]}"):
                return True
    return False


RET_SET = re.compile(r"(?<![\w.>])ret\s*=(?!=)\s*([^;]*);")


def sticky(stacks, after, until):
    """True when no line between `after` and `until` can turn a FALSE ret back to
    TRUE: every assignment of ret there other than `ret = FALSE` sits in a block
    `if (ret ...)` opened after `after`."""
    for number in range(after + 1, until):
        for match in RET_SET.finditer(code_only(stacks["lines"][number - 1])):
            if match[1].strip() == "FALSE":
                continue
            if not any(b[0] > after and guard(b) == ("ret",) for b in stacks[number]):
                return False
    return True


def runs_after(stacks, first_head, first_end, later_head):
    """True when the code at `later_head` runs only after the construct first_head ..
    first_end ran to its end with ret TRUE: `later_head` is inside a block `if (ret)`
    opened after the construct, ret cannot turn TRUE again on the way, and every
    condition of the construct that `later_head` does not share is a condition of
    `later_head` as well (or an `if (ret)` that keeps ret FALSE)."""
    a, b = stacks[first_head], stacks[later_head]
    checks = [x for x in b if x[0] > first_end and guard(x) == ("ret",) and sticky(stacks, first_end, x[0])]
    if not checks:
        return False
    check = checks[0][0]
    shared = {x[0] for x in b}
    for block in a:
        if block[0] in shared:
            continue
        g = guard(block)
        if g is None:
            continue
        if g == ("ret",):
            if not sticky(stacks, block[0], check):
                return False
            continue
        if g[0] in ("other", "elif") or not any(guard(y) and same_guard(g, guard(y)) for y in b):
            return False
    return True


def switch_body(lines, head, limit):
    """(selector, '{' line, end line, {label: [lines]}, default lines) of the switch
    at line `head`, or None. Labels with no statement between them share one group."""
    match = re.match(r"^\s*switch\s*\((.*)\)\s*\{?\s*$", code_only(lines[head - 1]))
    if not match:
        return None
    opened = head if "{" in code_only(lines[head - 1]) else head + 1
    depth, groups, default, current = 0, {}, [], None
    for number in range(opened, limit + 1):
        text = code_only(lines[number - 1])
        if depth == 1:
            case = re.match(r"^\s*case\s+([^:]+?)\s*:(.*)$", text)
            if case:
                label = case[1].strip()
                if current is None or current == "default" or any(groups[x] for x in current):
                    current = []
                current.append(label)
                groups[label] = groups.get(label, [])
                if case[2].strip():
                    for x in current:
                        groups[x].append(number)
            elif re.match(r"^\s*default\s*:", text):
                current = "default"
                default.append(number)
            elif text.strip() and current is not None:
                target = [default] if current == "default" else [groups[x] for x in current]
                for lines_of in target:
                    lines_of.append(number)
        elif depth > 1 and current is not None:
            target = [default] if current == "default" else [groups[x] for x in current]
            for lines_of in target:
                lines_of.append(number)
        depth += text.count("{") - text.count("}")
        if depth <= 0 and number >= opened:
            return norm(match[1]), opened, number, groups, default
    return None


def owning_switch(lines, first):
    """Head line of the switch whose `default:` arm holds line `first`, or None."""
    default = None
    for number in range(first, max(first - 4, 0), -1):
        if re.match(r"^\s*default\s*:", lines[number - 1]):
            default = number
            break
        if re.match(r"^\s*(case\b|\}|break;)", lines[number - 1]) and number != first:
            return None
    if default is None:
        return None
    head, depth = default - 1, 0
    while head > 0:
        text = code_only(lines[head - 1])
        depth += text.count("}") - text.count("{")
        if depth < 0:
            break
        head -= 1
    if head > 1 and code_only(lines[head - 1]).strip() == "{":
        head -= 1
    return head if head and re.match(r"^\s*switch\s*\(", lines[head - 1]) else None


def fails(lines, numbers):
    """True when the statements of a default arm end the switch with ret FALSE."""
    code = " ".join(code_only(lines[n - 1]) for n in numbers)
    sets = [m[1].strip() for m in RET_SET.finditer(code)]
    return (sets == ["FALSE"] or "return FALSE;" in code) and all(v == "FALSE" for v in sets)


def function_region(lines, first):
    """(definition line, last line) of the C function holding `first`, or None."""
    start = c_function_head(lines, first)
    if start is None:
        return None
    for number in range(start, len(lines) + 1):
        if lines[number - 1].startswith("}"):
            return start, number
    return None


def cov004(lines, first):
    """ASN1SCC-COV-004: the statement is in the `default:` arm of a switch on a
    selector read through the encoder's `const` pointer pVal, and an earlier switch
    of the same function on the same selector, whose default arm fails and whose
    labels are all labels of this switch, ran before it with ret TRUE."""
    head = owning_switch(lines, first)
    region = function_region(lines, first)
    if head is None or region is None:
        return False
    start, end = region
    if not re.search(r"\(\s*const\s+[\w ]+\*\s*pVal\s*,", lines[start - 1]):
        return False
    later = switch_body(lines, head, end)
    if later is None or not re.fullmatch(r"pVal->[\w.>-]+", later[0]):
        return False
    stacks = block_stacks(lines, start, end)
    stacks["lines"] = lines
    for number in range(start + 1, head):
        earlier = switch_body(lines, number, end)
        if earlier is None or earlier[0] != later[0] or earlier[2] >= head:
            continue
        if not set(earlier[3]) <= set(later[3]) or not fails(lines, earlier[4]):
            continue
        if runs_after(stacks, number, earlier[2], head):
            return True
    return False


def local_assignments(lines, start, end, name):
    """Right-hand sides of the assignments of local `name` in lines start..end."""
    pattern = re.compile(r"(?<![\w.>])" + re.escape(name) + r"\s*=(?!=)\s*([^;]*);")
    return [(n, m[1].strip()) for n in range(start, end + 1) for m in pattern.finditer(code_only(lines[n - 1]))]


def flag_follows_value(lines, stacks, start, end, name):
    """True when `name_is_initialized` starts FALSE and every `name_is_initialized = TRUE`
    follows an assignment of `name` in the same block (the update's case arm)."""
    flag = name + "_is_initialized"
    declared = any(re.search(r"\bflag\s+" + flag + r"\s*=\s*FALSE\s*;", lines[n - 1]) for n in range(start, end + 1))
    sets = [n for n, v in local_assignments(lines, start, end, flag) if v == "TRUE"]
    if not declared:
        return False
    for n in sets:
        block = stacks[n][-1][0] if stacks[n] else start
        if not any(m < n and m > block and stacks[m] == stacks[n] for m, _ in local_assignments(lines, start, end, name)):
            return False
    return True


def assigns_on_every_exit(lines, stacks, end, head, name):
    """For the construct starting at line `head`: a switch whose every case group assigns
    `name` and whose default arm fails, or an if / else if chain whose every branch
    assigns `name` but a final else that sets ret FALSE. Returns its last line or None."""
    def assigned(numbers, block):
        """An unconditional statement of the block (opened at line `block`) assigns `name`."""
        return any(re.match(r"^\s*" + re.escape(name) + r"\s*=(?!=)", code_only(lines[n - 1]))
                   and stacks[n] and stacks[n][-1][0] == block for n in numbers)
    body = switch_body(lines, head, end)
    if body is not None:
        _, opened, last, groups, default = body
        ok = groups and all(assigned(ns, opened) for ns in groups.values()) and default and fails(lines, default)
        return last if ok else None
    if not re.match(r"^\s*if\s*\(.*\)\s*\{\s*$", code_only(lines[head - 1])):
        return None
    branches, depth = [("if", head, [])], 1
    for number in range(head + 1, end + 1):
        text = code_only(lines[number - 1])
        branch = re.match(r"^\s*\}\s*(else(?:\s+if\s*\(.*\))?)\s*\{\s*$", text)
        if depth == 1 and branch:
            branches.append((branch[1], number, []))
            continue
        depth += text.count("{") - text.count("}")
        if depth == 0:
            break
        branches[-1][2].append(number)
    else:
        return None
    if branches[-1][0] != "else" or len(branches) < 2:
        return None
    if not all(assigned(ns, opened) for _, opened, ns in branches[:-1]) or not fails(lines, branches[-1][2]):
        return None
    return number


def cov005(lines, first):
    """ASN1SCC-COV-005: the statement is in the `default:` arm of a switch on a local
    of the encoder that every assignment sets to a label of that switch (or to a local
    set only to such labels and read under its _is_initialized flag), and the switch
    reads it only after it was assigned: under its _is_initialized flag, set only with
    the value, or after a construct that assigns it on every exit with ret TRUE."""
    head = owning_switch(lines, first)
    region = function_region(lines, first)
    if head is None or region is None:
        return False
    start, end = region
    if "_Encode" not in lines[start - 1]:
        return False
    body = switch_body(lines, head, end)
    if body is None or not re.fullmatch(r"[A-Za-z_]\w*", body[0]) or body[0] == "pVal":
        return False
    name, labels = body[0], set(body[3])
    declared = lambda v: any(re.match(r"^\s*[\w]+\s+" + re.escape(v) + r"\s*;", lines[n - 1]) for n in range(start, head))
    if not declared(name):
        return False
    stacks = block_stacks(lines, start, end)
    stacks["lines"] = lines
    for n, value in local_assignments(lines, start, end, name):
        if value in labels:
            continue
        # a source of a multi-determinant update: set only to labels, read only under its flag
        if not (re.fullmatch(r"\w+", value) and declared(value)
                and all(v in labels for _, v in local_assignments(lines, start, end, value))
                and flag_follows_value(lines, stacks, start, end, value)
                and any(guard(b) in (("if", value + "_is_initialized"), ("elif", value + "_is_initialized"))
                        for b in stacks[n])):
            return False
    if any(guard(b) == ("if", name + "_is_initialized") for b in stacks[head]) and flag_follows_value(lines, stacks, start, end, name):
        return True
    for number in range(start + 1, head):
        last = assigns_on_every_exit(lines, stacks, end, number, name)
        if last is not None and last < head and runs_after(stacks, number, last, head):
            return True
    return False


# RTL decoders whose postcondition bounds the value on success: the uPER
# constrained whole numbers (Result and IntVal in MinVal .. MaxVal) and the ACN
# fixed-size integers (Result.Success and IntVal in minVal .. maxVal).
ADA_RTL_DECODE = (r"\b(?:UPER_Dec_Constraint(?:Pos)?WholeNumber(?:U?Int(?:8|16|32))?|Dec_Constraint(?:Pos)?WholeNumber"
                  r"|Acn_Dec_(?:Int|UInt)_\w*ConstSize\w*)"
                  r"\s*\(\s*bs\s*,\s*{var}\s*,\s*(-?\d+)\s*,\s*(-?\d+)\s*(?:,\s*\d+\s*)?,"
                  r"\s*result(?:\.Success)?\s*\)")


def ada_code(text):
    """An Ada line without its comment."""
    return re.sub(r"--.*$", "", text)


def cov001_ada(lines, first):
    """ASN1SCC-COV-001 in Ada: the statement is in the `when others` arm of a
    `case X is` whose selector was just decoded by an RTL procedure with the
    postcondition `Result and X in min .. max`, and whose own choices are
    exactly min..max."""
    others = None
    for number in range(first, max(first - 4, 0), -1):
        text = ada_code(lines[number - 1])
        if re.match(r"^\s*when\s+others\s*=>", text, re.I):
            others = number
            break
        if re.match(r"^\s*(when\b|end\s+case\b)", text, re.I) and number != first:
            return False
    if others is None:
        return False
    head, depth = others - 1, 0
    while head > 0:
        text = ada_code(lines[head - 1])
        if re.match(r"^\s*end\s+case\s*;", text, re.I):
            depth += 1
        elif re.match(r"^\s*case\b.*\bis\s*$", text, re.I):
            if depth == 0:
                break
            depth -= 1
        head -= 1
    match = re.match(r"^\s*case\s+(\w+)\s+is\s*$", ada_code(lines[head - 1]), re.I) if head else None
    if not match:
        return False
    var = match[1]
    decode = re.compile(ADA_RTL_DECODE.format(var=re.escape(var)), re.I)
    assigned = re.compile(r"(?<![\w.])" + re.escape(var) + r"\s*:=", re.I)
    bounds = None
    for number in range(head - 1, max(head - 5, 0), -1):
        text = ada_code(lines[number - 1])
        found = decode.search(text)
        if found:
            bounds = int(found[1]), int(found[2])
            break
        if assigned.search(text):
            return False
    if bounds is None:
        return False
    # The case must sit in the success branch of that decode.
    if not any(re.match(r"^\s*if\s+result\.Success\s+then\s*$", ada_code(lines[n - 1]), re.I)
               for n in range(number + 1, head)):
        return False
    depth, labels = 0, []
    for number in range(head + 1, others):
        text = ada_code(lines[number - 1])
        if depth == 0:
            choice = re.match(r"^\s*when\s+(.*?)\s*=>", text, re.I)
            if choice:
                values = [v.strip() for v in choice[1].split("|")]
                if not all(re.fullmatch(r"-?\d+", v) for v in values):
                    return False  # a non-numeric choice: not the decoded-value shape
                labels += [int(v) for v in values]
        if re.match(r"^\s*case\b.*\bis\s*$", text, re.I):
            depth += 1
        elif re.match(r"^\s*end\s+case\s*;", text, re.I):
            depth -= 1
    return depth == 0 and covers(sorted(labels), bounds)


def proven_unreachable(language, lines, first, src):
    """ID of the documented exemption that proves the statement unreachable, or ''."""
    if language != "c":
        return "ASN1SCC-COV-001" if cov001_ada(lines, first) else ""
    if cov001(lines, first):
        return "ASN1SCC-COV-001"
    if cov002(lines, first, src):
        return "ASN1SCC-COV-002"
    if cov003(lines, first):
        return "ASN1SCC-COV-003"
    if cov004(lines, first):
        return "ASN1SCC-COV-004"
    if cov005(lines, first):
        return "ASN1SCC-COV-005"
    return ""


def function_starts(lines, language):
    """Sorted (line_number, name) of the function bodies in a source file."""
    starts = []
    for number, line in enumerate(lines, 1):
        if language == "c":
            if line[:1].isspace() or line.startswith(("#", "/", "}", "{")):
                continue
            match = C_FUNCTION.match(line)
            if match and match[1] not in {"if", "while", "for", "switch", "return", "sizeof"}:
                starts.append((number, match[1]))
        else:
            # Obligations are only reported for .adb bodies, where a nested
            # declaration is rare; the nearest preceding header is used.
            match = ADA_FUNCTION.match(line)
            if match:
                starts.append((number, match[1]))
    return starts


def enclosing(starts, line):
    name = "<file-level>"
    for number, candidate in starts:
        if number > line:
            break
        name = candidate
    return name


def configuration(run):
    config = json.loads((run / "manifest.json").read_text())["configuration"]
    parts = [config["language"], "acn-v2" if config["acn_v2"] else "legacy"]
    if config.get("slim"):
        parts.append("slim")
    if config.get("encodings", "both") != "both":
        parts.append(config["encodings"])
    return "/".join(parts), config["language"]


def classify(run):
    name, language = configuration(run)
    rows, units = [], []
    for record in map(json.loads, (run / "units.jsonl").read_text().splitlines()):
        work = run / record["directory"] / "work"
        exempt = bool(record.get("legacy_line_gate_exempt"))
        entry = {"config": name, "unit": record["unit"], "status": record["status"],
                 "exempt": exempt, "total": 0, "covered": 0}
        for file_name, data in record.get("files", {}).items():
            if data["component"] != "codec":
                continue
            entry["total"] += data["statements_total"]
            entry["covered"] += data["statements_covered"]
            source = work / file_name
            if not source.is_file():
                raise SystemExit(f"{run}: missing generated source {source}")
            lines = source.read_text(errors="replace").splitlines()
            starts = function_starts(lines, "c" if language == "c" else "Ada")
            for obligation in data["obligations"]:
                if obligation["status"] != "-":
                    continue
                spans = obligation["spans"]
                first, last = int(spans[0]["num"]), int(spans[-1]["num"])
                src = " ".join(s.get("src", "").strip() for s in spans)
                function = enclosing(starts, first)
                rows.append({"config": name, "unit": record["unit"], "exempt": exempt, "file": file_name,
                             "line": first, "function": function,
                             "function_kind": function_kind(function),
                             "statement_kind": statement_kind(src, lines, first, last),
                             "proven": proven_unreachable(language, lines, first, src),
                             "exempted": obligation.get("exempted"), "src": src[:200]})
        units.append(entry)
    return name, rows, units


def exemption_mismatches(rows):
    """Rows where the exemption region of the generated code (GNATcoverage) and the
    classifier's own check of the documented precondition disagree. Runs measured before
    the templates carried exemption regions have no `exempted` field and are not checked."""
    return [r for r in rows if r["exempted"] is not None and bool(r["exempted"]) != bool(r["proven"])]


def report_mismatches(outdir, rows):
    mismatches = exemption_mismatches(rows)
    if mismatches:
        with (outdir / "exemption-mismatches.csv").open("w", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=list(mismatches[0]))
            writer.writeheader()
            writer.writerows(mismatches)
        print(f"{len(mismatches)} obligations where exemption region and classifier disagree: "
              f"{outdir / 'exemption-mismatches.csv'}", file=sys.stderr)
    return mismatches


def table(counter, keys_a, keys_b, title_a, title_b):
    out = [f"| {title_a} | " + " | ".join(keys_b) + " | total |",
           "|---|" + "---:|" * (len(keys_b) + 1)]
    for a in keys_a:
        values = [counter[a, b] for b in keys_b]
        out.append(f"| {a} | " + " | ".join(map(str, values)) + f" | {sum(values)} |")
    values = [sum(counter[a, b] for a in keys_a) for b in keys_b]
    out.append("| **total** | " + " | ".join(map(str, values)) + f" | {sum(values)} |")
    return "\n".join(out)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("runs", nargs="+", type=Path, help="measurement directories (with units.jsonl)")
    ap.add_argument("--outdir", type=Path, required=True)
    ap.add_argument("--top", type=int, default=25)
    args = ap.parse_args(argv)
    args.outdir.mkdir(parents=True, exist_ok=False)
    rows, units, configs = [], [], []
    for run in args.runs:
        name, r, u = classify(run)
        configs.append(name)
        rows += r
        units += u
    with (args.outdir / "uncovered.csv").open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]) if rows else ["config"])
        writer.writeheader()
        writer.writerows(rows)
    with (args.outdir / "units.csv").open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=["config", "unit", "status", "exempt", "total", "covered"])
        writer.writeheader()
        writer.writerows(units)

    fkinds = sorted({r["function_kind"] for r in rows})
    skinds = [k + i for i in ("", "-ignored") for k in ("error", "default", "other")]
    report = ["# Uncovered generated-code statements by kind", "",
              "Source-pattern labels only; no reachability claim, except the `proven`",
              "column: statements that satisfy the precondition of a documented exemption",
              "(Docs/statement-coverage-exemptions.md), checked per statement.", "",
              "`exempt` = units with a NOCOVERAGE directive (exempt from the legacy line gate).",
              "`covered or proven` = (covered + proven unreachable) / total.", "",
              "| configuration | units | units ok / measured | statements covered / total | uncovered "
              "| proven unreachable | unproven | covered or proven |",
              "|---|---|---:|---:|---:|---:|---:|---:|"]
    summary = {}
    for name in configs:
        for scope, keep in (("all", lambda u: True), ("non-exempt", lambda u: not u["exempt"]),
                            ("exempt", lambda u: u["exempt"])):
            mine = [u for u in units if u["config"] == name and keep(u)]
            ok = [u for u in mine if u["status"] == "ok"]
            total, covered = sum(u["total"] for u in ok), sum(u["covered"] for u in ok)
            ok_units = {u["unit"] for u in ok}
            proven = sum(1 for r in rows if r["config"] == name and r["proven"]
                         and r["unit"] in ok_units and keep(r))
            summary[f"{name}|{scope}"] = {"units": len(mine), "units_ok": len(ok),
                                          "total": total, "covered": covered, "proven": proven}
            percent = lambda n: f"{100 * n / total if total else 0:.1f}%"
            report.append(f"| {name} | {scope} | {len(ok)} / {len(mine)} | {covered} / {total} "
                          f"({percent(covered)}) | {total - covered} | {proven} "
                          f"| {total - covered - proven} | {percent(covered + proven)} |")
    ids = sorted({r["proven"] for r in rows if r["proven"]})
    if ids:
        by_id = Counter((r["proven"], r["config"], r["function_kind"]) for r in rows if r["proven"])
        report += ["", "## Proven unreachable statements by exemption", "",
                   "| exemption | configuration | function kind | statements |", "|---|---|---|---:|"]
        report += [f"| {i} | {c} | {f} | {n} |" for (i, c, f), n in sorted(by_id.items())]
    for name in configs:
        for scope, keep in (("non-exempt units", False), ("exempt units", True)):
            mine = [r for r in rows if r["config"] == name and r["exempt"] == keep]
            counter = Counter((r["function_kind"], r["statement_kind"]) for r in mine)
            report += ["", f"## {name}, {scope}: function kind x statement kind", "",
                       table(counter, fkinds, skinds, "function kind", "statement kind")]
    everything = Counter((r["function_kind"], r["config"]) for r in rows)
    report += ["", "## All configurations: function kind x configuration", "",
               table(everything, fkinds, configs, "function kind", "configuration")]
    per_unit = Counter((r["config"], r["unit"], r["exempt"]) for r in rows)
    report += ["", f"## Top {args.top} units by uncovered statements", "",
               "| configuration | unit | exempt | uncovered |", "|---|---|---|---:|"]
    report += [f"| {c} | {u} | {'yes' if e else ''} | {n} |"
               for (c, u, e), n in per_unit.most_common(args.top)]
    patterns = Counter((r["function_kind"], r["statement_kind"],
                        re.sub(r"\b[A-Z][A-Za-z0-9_]*_([A-Za-z0-9_]+)\b", "<ID>", r["src"])[:90])
                       for r in rows if not r["exempt"])
    report += ["", f"## Top {args.top} statement patterns in non-exempt units (identifiers masked)", "",
               "| function kind | statement kind | pattern | count |", "|---|---|---|---:|"]
    report += [f"| {f} | {s} | `{p.replace('|', '/')}` | {n} |"
               for (f, s, p), n in patterns.most_common(args.top)]
    unknown = Counter(r["function"] for r in rows if r["function_kind"] == "other")
    if unknown:
        report += ["", "## Functions classified as `other`", ""]
        report += [f"- `{f}`: {n}" for f, n in unknown.most_common(args.top)]
    (args.outdir / "report.md").write_text("\n".join(report) + "\n")
    (args.outdir / "summary.json").write_text(json.dumps(
        {"configurations": summary,
         "by_kind": {f"{c}|{'exempt' if e else 'non-exempt'}|{f}|{s}": n for (c, e, f, s), n in
                     Counter((r["config"], r["exempt"], r["function_kind"], r["statement_kind"])
                             for r in rows).items()}},
        indent=2, sort_keys=True) + "\n")
    print(args.outdir / "report.md")
    return 1 if report_mismatches(args.outdir, rows) else 0


if __name__ == "__main__":
    sys.exit(main())
