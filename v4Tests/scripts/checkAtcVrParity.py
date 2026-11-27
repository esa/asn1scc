#!/usr/bin/env python3
"""Ada / C parity of the guarded Equal checks of the automatic tests (coverage cycle 15i).

The Equal tests compare a lifted "another value" variant only at a node they can compare exactly (macro
equalityVariantCheckGuarded, DastTestCaseCreation.activeDiff). The rule is the same for C and Ada, except that Ada must
also name each CHOICE selector literal through the package that declares it, and leaves a CHOICE node out when it
cannot prove that package. A wrong Ada decision there either fails the Ada build (a literal through the wrong package)
or silently drops checks; this script catches the second: for every test directive (--TCLS / --TCFS) of the given
corpus files, legacy and --acn-v2, it generates the C and the Ada automatic tests with the flags of runTests.py and
requires the same multiset of descriptions of the guarded checks that compare a CHOICE selector, in both. (The other
guarded checks may differ by design: C has toggles of one-value scalars and of single bits that Ada has not.)

Usage: checkAtcVrParity.py FILE.asn1 ...   (ASN1SCC = compiler, as for runTests.py; JOBS = parallel compiler runs,
default the number of CPUs). Exit 1 on any difference or compiler failure.
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


def directives(asn1):
    """(label, acn content or None, acn file or None) of the directives that must pass (--TCLS, --TCFS)."""
    for line in open(asn1, errors="replace"):
        if line.startswith("--TCLS"):
            content = line.split("--TCLS")[1].strip()
            yield content, content, None
        elif line.startswith("--TCFS"):
            name = line.split("--TCFS")[1].strip()
            yield name, None, os.path.join(os.path.dirname(os.path.abspath(asn1)), name)


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


SELECTOR = {"c": re.compile(r"\bkind\s*!?=="), "Ada": re.compile(r"\.kind\s*=")}


def descriptions(outdir, lang):
    """Descriptions of the guarded checks whose comparison reads a CHOICE selector."""
    found = collections.Counter()
    for name in sorted(os.listdir(outdir)):
        if name.startswith("test_case_") and name.endswith(".c" if lang == "c" else ".adb"):
            lines = open(os.path.join(outdir, name), errors="replace").read().splitlines()
            for i, line in enumerate(lines):
                m = GUARDED[lang].match(line)
                if m and SELECTOR[lang].search(comparison(lines, i, lang)):
                    found[m.group(1)] += 1
    return found


def compare(work, asn1, k, label, content, acnFile, mode):
    """One configuration: (result line(s), compared, failed)."""
    found = {}
    for lang in ("c", "Ada"):
        out = os.path.join(work, f"{os.path.basename(asn1)}-{k}-{mode}-{lang}")
        os.makedirs(out)
        acn = acnFile
        if content is not None:
            acn = os.path.join(out, "sample1.acn")
            with open(acn, "w") as f:
                f.write("TEST-CASE DEFINITIONS ::= BEGIN\n\t" + content + "\nEND\n")
        cmd = [COMPILER, "-" + lang, *FLAGS] + (["--acn-v2"] if mode == "acn-v2" else []) + \
              ["-o", out, os.path.abspath(asn1), acn]
        r = subprocess.run(cmd, capture_output=True, text=True)
        if r.returncode != 0:
            return f"FAILED {asn1} {label} {mode} {lang}: compiler exit {r.returncode}: {r.stderr.strip()[:300]}", 0, 1
        found[lang] = descriptions(out, lang)
    onlyC, onlyAda = found["c"] - found["Ada"], found["Ada"] - found["c"]
    if onlyC or onlyAda:
        lines = [f"FAILED {asn1} {label} {mode}: guarded CHOICE checks C {sum(found['c'].values())}, "
                 f"Ada {sum(found['Ada'].values())}"]
        lines += [f"    C only:   {what}" for what in sorted(onlyC)]
        lines += [f"    Ada only: {what}" for what in sorted(onlyAda)]
        return "\n".join(lines), 1, 1
    return f"OK {asn1} {label} {mode}: {sum(found['c'].values())} guarded CHOICE checks in C and Ada", 1, 0


def main(files):
    jobs = []
    for asn1 in files:
        # the first-line markers of runTests.py: C_ONLY files have no Ada tests, ACNV2_ONLY files no legacy run
        with open(asn1, errors="replace") as f:
            markers = f.readline()
        if "C_ONLY" in markers:
            continue
        modes = ("acn-v2",) if "ACNV2_ONLY" in markers else ("legacy", "acn-v2")
        for k, (label, content, acnFile) in enumerate(directives(asn1)):
            jobs += [(asn1, k, label, content, acnFile, mode) for mode in modes]
    bad = checked = 0
    with tempfile.TemporaryDirectory(prefix="atc-vr-parity-") as work, \
            concurrent.futures.ThreadPoolExecutor(int(os.environ.get("JOBS", os.cpu_count() or 1))) as pool:
        for text, c, b in pool.map(lambda j: compare(work, *j), jobs):
            print(text, flush=True)
            checked += c
            bad += b
    print(f"{checked} configurations compared, {bad} failed")
    return 1 if bad or checked == 0 else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
