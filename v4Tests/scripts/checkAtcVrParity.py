#!/usr/bin/env python3
"""Ada / C parity of the CHOICE selector comparisons in the guarded Equal checks of the automatic tests
(coverage cycle 15i).

The Equal tests compare a lifted "another value" variant only at a node they can compare exactly (macro
equalityVariantCheckGuarded, DastTestCaseCreation.activeDiff). The rule is the same for C and Ada, except that Ada must
name each CHOICE selector literal through the package that declares it and leaves a CHOICE node out when it cannot
prove that package; C names no package, so it has no such rule. A wrong Ada decision either fails the Ada build or
silently drops comparisons; this script catches the second. For every test directive (--TCLS / --TCFS) of the given
corpus files, legacy and --acn-v2 (first-line markers C_ONLY / ACNV2_ONLY as in runTests.py), it generates the C and
the Ada automatic tests with the flags of runTests.py and requires, per guarded check (its description), the same
multiset of selector comparisons in both, each identified by its normalised path:

  same selector   C   if (<p>kind != <q>kind) { tc__differs = TRUE; }       Ada  <p>.kind = <q>.kind
  selected arm    C   else if (<p>kind == <ALT>_PRESENT) {                   Ada  <p>.kind = Standard.<pkg>.<ALT>_PRESENT then

(templates atcDiffChoice / atcDiffChoiceAlternative of StgC/test_cases_c.stg and StgAda/test_cases_a.stg). Paths are
normalised to one form: C `.arr[<index>]` and Ada `.Data (<index>)` become `[]`, and `.u` (the C union of a CHOICE)
is removed in both languages. The "same selector" form is also the comparison of a scalar, so a SEQUENCE component
named `kind` is counted too, as the selector of its SEQUENCE: in both languages alike, and at the path of a node that
is not a CHOICE, so it can neither make the languages differ nor stand in for a selector comparison of a CHOICE. The other guarded checks may differ by design (C toggles one-value scalars
and single bits, Ada does not) and are not compared.

Usage: checkAtcVrParity.py [--self-test] FILE.asn1 ...
  ASN1SCC = compiler (as for runTests.py), JOBS = parallel compiler runs (default: the number of CPUs).
  --self-test also runs the built-in checks of the extraction (a generated grammar and a mutant that must fail).
Exit 1 on any difference, compiler failure or failed self-test.
"""
import collections
import concurrent.futures
import os
import re
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
COMPILER = os.environ.get("ASN1SCC", os.path.join(ROOT, "..", "asn1scc", "bin", "Debug", "net10.0", "asn1scc"))
FLAGS = ["-uPER", "-ACN", "-ig", "-typePrefix", "ASN1SCC_", "-renamePolicy", "3", "-fp", "AUTO", "-equal", "-atc"]
GUARDED = {
    "c": re.compile(r"^\s*/\* (.*) \(checked when the change wrote another value at the node\) \*/\s*$"),
    "Ada": re.compile(r"^\s*--  (.*) \(checked when the change wrote another value at the node\)\s*$"),
}
C_PATH = r"(?:[\w.]|\[[^\]]*\])*?"
ADA_PATH = r"(?:[\w.]|\s?\(\w+\))*?"
SELECTOR = {
    "c": [("same", re.compile(r"if \((tc_data\b" + C_PATH + r")kind != tc_data2\b" + C_PATH +
                              r"kind\) \{ tc__differs = TRUE; \}")),
          ("arm", re.compile(r"else if \((tc_data\b" + C_PATH + r")kind == (\w+)\) \{"))],
    "Ada": [("same", re.compile(r"(tc_data\b" + ADA_PATH + r")\.kind = tc_data2\b" + ADA_PATH + r"\.kind\b")),
            ("arm", re.compile(r"(tc_data\b" + ADA_PATH + r")\.kind = Standard\.\w+\.(\w+) then\b"))],
}


def normalise(path, lang):
    p = path.replace(" ", "").rstrip(".")
    p = re.sub(r"\.arr\[[^\]]*\]", "[]", p) if lang == "c" else re.sub(r"\.Data\(\w+\)", "[]", p)
    return re.sub(r"\.u(?=\.|$)", "", p)


def comparison(lines, start, lang):
    """The comparison text (sDiff) of the guarded check whose description is lines[start]."""
    if lang == "c":
        # tc__differs = FALSE; <sDiff> if (tc__differs) {
        first = next(i for i in range(start, len(lines)) if lines[i].strip() == "tc__differs = FALSE;")
        last = next(i for i in range(first, len(lines)) if lines[i].strip() == "if (tc__differs) {")
        return "\n".join(lines[first + 1:last])
    # if <sDiff> then <skip>: the last `if ` at the indentation of the description before the skip message
    indent = len(lines[start]) - len(lines[start].lstrip())
    skip = next(i for i in range(start, len(lines)) if "check skipped (the change wrote the same value" in lines[i])
    first = max(i for i in range(start, skip) if lines[i].startswith(" " * indent + "if "))
    return "\n".join(lines[first:skip])


def selectors(text, lang):
    """The normalised selector comparisons of one comparison text."""
    found = []
    for kind, rx in SELECTOR[lang]:
        for m in rx.finditer(text):
            found.append((kind, normalise(m.group(1), lang)) + ((m.group(2).lower(),) if kind == "arm" else ()))
    return found


def test_files(outdir, lang):
    return [os.path.join(outdir, n) for n in sorted(os.listdir(outdir))
            if n.startswith("test_case_") and n.endswith(".c" if lang == "c" else ".adb")]


def extract(outdir, lang):
    """Counter of (description, selector comparison) over the guarded checks of the test files."""
    found = collections.Counter()
    for path in test_files(outdir, lang):
        lines = open(path, errors="replace").read().splitlines()
        for i, line in enumerate(lines):
            m = GUARDED[lang].match(line)
            if m:
                for sel in selectors(comparison(lines, i, lang), lang):
                    found[(m.group(1), sel)] += 1
    return found


def differences(cdir, adadir):
    """The selector comparisons C and Ada do not share (empty: parity holds), and the totals."""
    c, ada = extract(cdir, "c"), extract(adadir, "Ada")
    lines = [f"    C only:   {what}: {sel}" for (what, sel) in sorted((c - ada).elements())]
    lines += [f"    Ada only: {what}: {sel}" for (what, sel) in sorted((ada - c).elements())]
    return lines, sum(c.values()), sum(ada.values())


def generate(work, asn1, content, acnFile, mode, lang):
    out = os.path.join(work, lang)
    os.makedirs(out)
    acn = acnFile
    if content is not None:
        acn = os.path.join(out, "sample1.acn")
        with open(acn, "w") as f:
            f.write("TEST-CASE DEFINITIONS ::= BEGIN\n\t" + content + "\nEND\n")
    cmd = [COMPILER, "-" + lang, *FLAGS] + (["--acn-v2"] if mode == "acn-v2" else []) + \
          ["-o", out, os.path.abspath(asn1), acn]
    return out, subprocess.run(cmd, capture_output=True, text=True)


def directives(asn1):
    """(label, acn content or None, acn file or None) of the directives that must pass (--TCLS, --TCFS)."""
    for line in open(asn1, errors="replace"):
        if line.startswith("--TCLS"):
            content = line.split("--TCLS")[1].strip()
            yield content, content, None
        elif line.startswith("--TCFS"):
            name = line.split("--TCFS")[1].strip()
            yield name, None, os.path.join(os.path.dirname(os.path.abspath(asn1)), name)


def compare(workdir, asn1, label, content, acnFile, mode):
    """One configuration in its own work dir: (report text, compared, failed)."""
    dirs = {}
    for lang in ("c", "Ada"):
        dirs[lang], r = generate(workdir, asn1, content, acnFile, mode, lang)
        if r.returncode != 0:
            return f"FAILED {asn1} {label} {mode} {lang}: compiler exit {r.returncode}: {r.stderr.strip()[:300]}", 0, 1
    diff, nc, na = differences(dirs["c"], dirs["Ada"])
    if diff:
        return "\n".join([f"FAILED {asn1} {label} {mode}: selector comparisons C {nc}, Ada {na}"] + diff), 1, 1
    return f"OK {asn1} {label} {mode}: {nc} selector comparisons in C and Ada", 1, 0


SELF_TEST_GRAMMAR = """TEST-CASE DEFINITIONS AUTOMATIC TAGS ::= BEGIN
  IMPORTS Box FROM Other-Mod;
  Outer ::= SEQUENCE { x Box, l SEQUENCE (SIZE(1..2)) OF Box, f BOOLEAN }
END
Other-Mod DEFINITIONS AUTOMATIC TAGS ::= BEGIN
  Box ::= SEQUENCE { c CHOICE { a NULL, b NULL }, kind BOOLEAN,
                     s CHOICE { p CHOICE { m INTEGER (0..3), n BOOLEAN }, q BOOLEAN }, g BOOLEAN }
END
"""


def self_test(work):
    """Checks of the extraction on generated output (C and Ada, legacy); returns the number of failed checks."""
    asn1 = os.path.join(work, "selftest.asn1")
    with open(asn1, "w") as f:
        f.write(SELF_TEST_GRAMMAR)
    bad = 0

    def check(cond, text):
        nonlocal bad
        print(("OK " if cond else "FAILED ") + "self-test: " + text)
        bad += 0 if cond else 1

    dirs = {}
    for lang in ("c", "Ada"):
        dirs[lang], r = generate(os.path.join(work, "selftest"), asn1, "Outer[]", None, "legacy", lang)
        if r.returncode != 0:
            check(False, f"generation ({lang}): {r.stderr.strip()[:300]}")
            return bad
    c, ada = extract(dirs["c"], "c"), extract(dirs["Ada"], "Ada")

    def sels(found, desc):
        return sorted(sel for (what, sel) in found.elements() if what == desc)

    # a CHOICE of NULL alternatives has only the "same selector" comparison (C: `!=`)
    d = "Outer.x.c: another alternative"
    check(sels(c, d) == sels(ada, d) == [("same", "tc_data.x.c")],
          f"'{d}' (CHOICE {{ a NULL, b NULL }}): C {sels(c, d)}, Ada {sels(ada, d)}")
    # a BOOLEAN component named kind: read as the selector of its SEQUENCE x, the same in both languages (x is no
    # CHOICE, so no selector comparison of x can be confused with it)
    d = "Outer.x.kind: another value"
    check(sels(c, d) == sels(ada, d) == [("same", "tc_data.x")],
          f"'{d}' (a component named kind): C {sels(c, d)}, Ada {sels(ada, d)}")
    # nested CHOICE inside an alternative, SEQUENCE OF elements: the normalised paths agree
    check(c == ada and ("same", "tc_data.x.s.p") in {sel for (_, sel) in c}
          and any("[]" in sel[1] for (_, sel) in c),
          f"nested CHOICE and SEQUENCE OF element paths: {sum(c.values())} comparisons, C and Ada agree")
    # mutant: one of several selector comparisons of a check replaced by True in Ada must be reported
    desc = next(what for (what, _) in sorted(c) if len(sels(c, what)) >= 2)
    mutated = False
    for path in test_files(dirs["Ada"], "Ada"):
        lines = open(path).read().splitlines()
        for i, line in enumerate(lines):
            m = GUARDED["Ada"].match(line)
            if m and m.group(1) == desc and not mutated:
                text = comparison(lines, i, "Ada")
                hit = SELECTOR["Ada"][0][1].search(text)
                whole = "\n".join(lines).replace(text, text[:hit.start()] + "True" + text[hit.end():], 1)
                with open(path, "w") as f:
                    f.write(whole + "\n")
                mutated = True
    diff, _, _ = differences(dirs["c"], dirs["Ada"])
    check(mutated and diff != [], f"mutant (one selector comparison of '{desc}' replaced by True in Ada) is reported")
    return bad


def main(argv):
    selfTest = "--self-test" in argv
    files = [a for a in argv if a != "--self-test"]
    jobs = []
    for asn1 in files:
        with open(asn1, errors="replace") as f:
            markers = f.readline()
        if "C_ONLY" in markers:
            continue
        modes = ("acn-v2",) if "ACNV2_ONLY" in markers else ("legacy", "acn-v2")
        for label, content, acnFile in directives(asn1):
            jobs += [(asn1, label, content, acnFile, mode) for mode in modes]
    bad = checked = 0
    with tempfile.TemporaryDirectory(prefix="atc-vr-parity-") as work:
        if selfTest:
            bad += self_test(work)
        # one work dir per configuration (files of the same name in different folders)
        with concurrent.futures.ThreadPoolExecutor(int(os.environ.get("JOBS", os.cpu_count() or 1))) as pool:
            for text, c, b in pool.map(lambda nj: compare(os.path.join(work, str(nj[0])), *nj[1]), enumerate(jobs)):
                print(text, flush=True)
                checked += c
                bad += b
    print(f"{checked} configurations compared, {bad} failed")
    return 1 if bad or (checked == 0 and not selfTest) else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
