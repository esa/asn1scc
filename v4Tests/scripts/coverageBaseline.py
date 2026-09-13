#!/usr/bin/env python3
"""Generated-code coverage measurement, derived from Phase 0 (f5d4412b).

Python standard library only. The companion Dockerfile supplies the compiler,
GCC/gcov and GNAT. Source-pattern categories never imply unreachability.
"""
import argparse
from collections import Counter, defaultdict
from datetime import datetime, timezone
import gzip
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shutil
import signal
import subprocess
import sys
import time
import uuid

V4 = Path(__file__).resolve().parent.parent
SCHEMA = 1
METRICS = ("lines_total", "lines_hit", "branches_total", "branches_taken",
           "blocks_total", "blocks_hit", "partially_executed_lines",
           "legacy_excluded_lines", "legacy_excluded_lines_hit")
PILOT = (
    "01-INTEGER/001.asn1#1", "04-ENUMERATED/001.asn1#1",
    "03-IA5String/014.asn1#1", "06-OCTET-STRING/004.asn1#2",
    "08-BIT-STRING/002.asn1#3", "09-CHOICE/003.asn1#3",
    "10-SEQEUENCE/008.asn1#1", "11-SEQUENCE-OF/005.asn1#1",
    "12-PARAM/001_01.asn1#1", "23-CONTAINING/003.asn1#1",
    "24-DEDUCED-SIZE/003.asn1#1", "24-DEDUCED-SIZE/004.asn1#1",
)


def write_json(path, value):
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")


def digest(value):
    return hashlib.sha256(value).hexdigest()


def file_hash(path):
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def tree_manifest(root):
    excluded = {".git", "bin", "obj", "__pycache__", "target"}
    entries = {p.relative_to(root).as_posix(): file_hash(p)
               for p in sorted(root.rglob("*"))
               if p.is_file() and not p.is_symlink()
               and not excluded.intersection(p.relative_to(root).parts)}
    return {"sha256": digest(json.dumps(entries, sort_keys=True).encode()),
            "files": entries}


def input_bytes(unit, root):
    asn1 = (root / unit["asn1"]).read_bytes()
    if unit["acn_kind"] == "inline":
        acn = ("-- Auto generated file\n\nTEST-CASE DEFINITIONS ::= BEGIN\n\t"
               + unit["acn_payload"] + "\nEND\n").encode()
    else:
        path = (root / unit["asn1"]).parent / unit["acn_payload"]
        path = path.resolve()
        if not path.is_relative_to(root.resolve()):
            raise ValueError("ACN input must stay inside the test corpus")
        acn = path.read_bytes()
    return asn1, acn


def input_hash(unit, root):
    asn1, acn = input_bytes(unit, root)
    return digest(asn1 + b"\0ACN\0" + acn)


def enumerate_units(root):
    units = []
    for path in sorted(root.rglob("*.asn1")):
        lines = path.read_text(encoding="utf-8-sig").splitlines()
        first = lines[0] if lines else ""
        ordinal = 0
        for number, line in enumerate(lines, 1):
            for marker, kind in (("--TCLS", "inline"), ("--TCFS", "file")):
                if line.startswith(marker):
                    ordinal += 1
                    relative = path.relative_to(root).as_posix()
                    unit = {"unit": f"{relative}#{ordinal}", "asn1": relative,
                            "directive_line": number, "acn_kind": kind,
                            "acn_payload": line[len(marker):].strip(),
                            "nocoverage": "NOCOVERAGE" in first,
                            "no_atc": "NO_AUTOMATIC_TEST_CASES" in first}
                    try:
                        unit["input_sha256"] = input_hash(unit, root)
                    except (OSError, ValueError) as error:
                        unit["input_error"] = str(error)
                    units.append(unit)
                    break
    return units


def select_units(units, args, reference):
    ids = {u["unit"] for u in units}
    cohort = set(PILOT) if args.cohort == "pilot" else (
        set(reference["units"]) if args.cohort == "historical" else ids)
    missing = cohort - ids
    if missing:
        raise ValueError("Missing cohort units: " + ", ".join(sorted(missing)))
    selected = 0
    for unit in units:
        if unit["no_atc"]:
            reason = "NO_AUTOMATIC_TEST_CASES"
        elif unit["unit"] not in cohort:
            reason = "outside_cohort"
        elif args.filter and args.filter not in unit["unit"]:
            reason = "filter"
        elif args.limit and selected >= args.limit:
            reason = "limit"
        else:
            reason = "selected"
            selected += 1
        unit["selection"] = reason
    return [u for u in units if u["selection"] == "selected"]


def classify_arm(function, text):
    """Historical B1-B8 taxonomy; only a source-pattern label."""
    fn = function.lower()
    if "COVERAGE_IGNORE" in text or "_is_initialized" in text or "UNINITIALIZED" in text:
        return "B6"
    if "currentBit == 0" in text:
        return "B7"
    if "isconstraintvalid" in fn or "charsarevalid" in fn:
        return "B2"
    if fn.endswith("_equal"):
        return "B5"
    if "_decode" in fn:
        return "B4" if "isconstraintvalid" in text.lower() else "B1"
    if "_encode" in fn:
        return "B3" if "bCheckConstraints" in text or "*pErrCode == 0" in text else "B8"
    return "OTHER"


def function_group(function):
    fn = function.lower()
    if "isconstraintvalid" in fn or "charsarevalid" in fn:
        return "validation"
    if fn.endswith("_equal"):
        return "equality"
    if "_initialize" in fn or "_constant" in fn or fn.endswith("_init"):
        return "initialization"
    if "_acn_" in fn:
        return "ACN"
    if "_encode" in fn or "_decode" in fn:
        return "UPER"
    return "other"


def file_component(name):
    name = name.lower()
    if name.startswith(("asn1crt", "adaasn1rtl", "board_config")):
        return "rtl"
    if (name.startswith(("mainprogram", "testsuite", "b__"))
            or "_auto_tcs" in name
            or re.match(r"test_case_\d", name)):
        return "harness"
    return "codec"


def exclusion_reason(text, language):
    if "COVERAGE_IGNORE" in text:
        return "legacy_COVERAGE_IGNORE_not_a_reachability_proof"
    trivia = {"}", "default:", "break;"} if language == "c" else {
        "end;", "declare", "default:", "break;"}
    return "legacy_trivia" if text.strip() in trivia else None


def text_gcov_misses(text, language, literal_legacy=False):
    misses = []
    for line in text.splitlines():
        parts = line.split(":", 2)
        uncovered = "####" in line if literal_legacy else "####" in parts[0]
        if uncovered:
            if len(parts) == 3 and not exclusion_reason(parts[2], language):
                misses.append(int(parts[1]))
    return misses


def source_text_report(entry, reports, name):
    if name in reports:
        return reports[name]
    if not entry["lines"] and not entry.get("functions"):
        return ""  # gcov omits text for empty source mappings within real objects.
    raise StageError("gcov", f"Missing text gcov for {name}")


def metrics():
    return dict.fromkeys(METRICS, 0)


def analyse_file(entry, source, identity, language):
    """Keep individual arms and partial-line information, with exact-source IDs."""
    lines = source.read_text().splitlines()
    source_sha = file_hash(source)
    result = {**metrics(), "source_sha256": source_sha, "lines": [], "branches": [],
              "functions": entry.get("functions", []), "official_line_misses": [],
              "groups": {}, "taxonomy": {}, "branch_exclusions": []}
    groups = defaultdict(metrics)
    taxonomy = Counter()
    for ln in entry["lines"]:
        number = ln["line_number"]
        if not 0 < number <= len(lines):
            raise ValueError(f"gcov references missing source line {number}")
        text = lines[number - 1]
        function = ln.get("function_name", "")
        group = groups[function_group(function)]
        reason = exclusion_reason(text, language)
        hit = int(ln["count"] > 0)
        partial = int(hit and ln.get("unexecuted_block", False))
        for target in (result, group):
            target["lines_total"] += 1
            target["lines_hit"] += hit
            target["partially_executed_lines"] += partial
            target["legacy_excluded_lines"] += int(reason is not None)
            target["legacy_excluded_lines_hit"] += hit * int(reason is not None)
        if not hit and not reason:
            result["official_line_misses"].append(number)
        result["lines"].append({**ln, "text": text, "legacy_exclusion": reason})
        for index, arm in enumerate(ln.get("branches", [])):
            taken = int(arm["count"] > 0)
            key = [identity, source_sha, function, number, index]
            result["branches"].append({
                **arm, "id": digest(json.dumps(key).encode()),
                "function": function, "line": number, "ordinal": index, "text": text,
                "pattern_category": classify_arm(function, text),
                "reachability": "observed" if taken else "unresolved",
                "proof": None, "semantic_goal_id": None})
            for target in (result, group):
                target["branches_total"] += 1
                target["branches_taken"] += taken
            if not taken:
                taxonomy[classify_arm(function, text)] += 1
    for function in entry.get("functions", []):
        if "blocks" not in function or "blocks_executed" not in function:
            raise ValueError("gcov JSON lacks required function basic-block counts")
        for target in (result, groups[function_group(function["name"])]):
            target["blocks_total"] += function["blocks"]
            target["blocks_hit"] += function["blocks_executed"]
    result["groups"] = dict(groups)
    result["taxonomy"] = dict(taxonomy)
    return result


class StageError(Exception):
    def __init__(self, stage, detail, timeout=False):
        self.stage, self.detail, self.timeout = stage, detail, timeout
        super().__init__(detail)


def validate_profile(payload, has_data, diagnostics):
    """Empty compiler translation units legitimately have no runtime counters."""
    empty = not any(e["lines"] or e.get("functions") for e in payload["files"])
    if not empty and not has_data:
        raise StageError("gcov", "Missing runtime data for an instrumented object")
    allowed = ("no functions found", "cannot open data file, assuming not executed")
    if diagnostics.strip() and not (empty and all(
            line.endswith(allowed) for line in diagnostics.strip().splitlines())):
        raise StageError("gcov", diagnostics)
    return empty


def run_step(command, cwd, logs, stage, timeout, steps, strict_stderr=False):
    start = time.monotonic()
    stdout_path, stderr_path = logs / (stage + ".stdout"), logs / (stage + ".stderr")
    step = {"stage": stage, "command": [str(x) for x in command], "cwd": str(cwd)}
    steps.append(step)
    try:
        with stdout_path.open("wb") as out, stderr_path.open("wb") as err:
            process = subprocess.Popen(command, cwd=cwd, stdout=out, stderr=err,
                                       start_new_session=True)
            try:
                step["returncode"] = process.wait(timeout=timeout)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
                step["timed_out"] = True
                raise StageError(stage, f"Exceeded {timeout}s", timeout=True)
            except BaseException:
                if process.poll() is None:
                    os.killpg(process.pid, signal.SIGKILL)
                    process.wait()
                raise
    except OSError as error:
        raise StageError(stage, str(error)) from error
    finally:
        step["seconds"] = round(time.monotonic() - start, 3)
    if step["returncode"] or (strict_stderr and stderr_path.stat().st_size):
        detail = (stderr_path.read_text(errors="replace")
                  or stdout_path.read_text(errors="replace"))[-2000:]
        raise StageError(stage, detail or f"Exit status {step['returncode']}")
    return stdout_path.read_text(errors="replace")


def compiler_flags(args):
    enc = {"both": ["-uPER", "-ACN"], "uper": ["-uPER"], "acn": ["-ACN"]}[args.encodings]
    flags = ["-" + args.language, *enc, "-ig", "-typePrefix", "ASN1SCC_",
             "-renamePolicy", "3", "-fp", "AUTO", "-equal", "-atc"]
    if args.acn_v2:
        flags.append("--acn-v2")
    if args.slim:
        flags.append("-slim")
    # Preserve the original default flags exactly for the historical cohort.
    if args.word_size != 8:
        flags.extend(["-wordSize", "4", "-fpWordSize", "4", "-t", "msp430"])
    return flags


def measure_unit(unit, args, run_dir):
    token = digest(unit["unit"].encode())[:16]
    directory = run_dir / "units" / token
    work, logs, raw = directory / "work", directory / "logs", directory / "gcov"
    for folder in (work, logs, raw):
        folder.mkdir(parents=True, exist_ok=False)
    rec = {"unit": unit["unit"], "status": "ok", "nocoverage": unit["nocoverage"],
           "input_sha256": unit.get("input_sha256"), "directory": str(directory.relative_to(run_dir)),
           "steps": [], "files": {}, "source_omissions": []}
    start = time.monotonic()
    try:
        if "input_error" in unit:
            raise StageError("setup", unit["input_error"])
        asn1, acn = input_bytes(unit, args.test_root)
        (work / "sample1.asn1").write_bytes(asn1)
        (work / "sample1.acn").write_bytes(acn)
        run_step([str(args.compiler), *compiler_flags(args), "-o", str(work),
                  "sample1.asn1", "sample1.acn"], work, logs, "compile", args.timeout,
                 rec["steps"], strict_stderr=True)
        if args.language == "c":
            run_step(["make", f"-j{args.jobs}", f"CC={args.gcc}"], work, logs,
                     "build", args.timeout, rec["steps"])
            executable = work / "mainprogram"
            expected = {p.name for p in work.glob("*.c") if file_component(p.name) == "codec"}
        else:
            command = ["gprbuild", "-P", "asn1_x86.gpr", "mainprogram.adb",
                       "--subdirs=coverage", f"-j{args.jobs}", "-cargs", "-gnat2012",
                       "-g", "-O0", "-gnatf", "-gnaty", "-gnatg", "-fstack-check",
                       "-gnatwe", "-gnatwa", "-fprofile-arcs", "-ftest-coverage",
                       "-largs", "-fprofile-arcs"]
            run_step(command, work, logs, "build", args.timeout, rec["steps"])
            executable = work / "obj_x86" / "coverage" / "mainprogram"
            expected = {p.name for p in work.glob("*.adb") if file_component(p.name) == "codec"}
        run_step([str(executable)], executable.parent, logs, "run", args.timeout,
                 rec["steps"], strict_stderr=True)
        gcnotes = sorted(work.rglob("*.gcno"))
        if not gcnotes or not list(work.rglob("*.gcda")):
            raise StageError("gcov", "Missing instrumentation or runtime profile")
        observed = set()
        for index, note in enumerate(gcnotes):
            gcdir = raw / f"{index:03d}-{note.stem}"
            gcdir.mkdir()
            run_step([args.gcov, "-b", "--json-format", str(note)], note.parent, logs,
                     f"gcov-json-{index}", args.timeout, rec["steps"])
            payloads = list(note.parent.glob("*.gcov.json.gz"))
            if len(payloads) != 1:
                raise StageError("gcov", "Expected exactly one JSON artifact per object")
            with gzip.open(payloads[0], "rt") as stream:
                payload = json.load(stream)
            shutil.move(str(payloads[0]), gcdir / payloads[0].name)
            diagnostics = (logs / f"gcov-json-{index}.stderr").read_text()
            if validate_profile(payload, note.with_suffix(".gcda").exists(), diagnostics):
                rec["source_omissions"].append({
                    "object": note.relative_to(work).as_posix(),
                    "reason": "empty_translation_unit_no_executable_code",
                    "gcno_sha256": file_hash(note), "lines": 0, "branches": 0})
                for source in work.glob(note.stem + ".*"):
                    if source.suffix not in (".c", ".adb", ".ads"):
                        continue
                    key = source.name + "@" + note.relative_to(work).as_posix()
                    stat = analyse_file({"lines": [], "functions": []}, source, key, args.language)
                    stat.update(source=source.name, component=file_component(source.name),
                                text_gcov_misses=[], json_line_misses=[],
                                legacy_grep_false_positives=[])
                    rec["files"][key] = stat
                    observed.add(source.name)
                continue
            run_step([args.gcov, "-b", "-c", "-p", str(note)], note.parent, logs,
                     f"gcov-text-{index}", args.timeout, rec["steps"], strict_stderr=True)
            for generated in note.parent.glob("*.gcov"):
                shutil.move(str(generated), gcdir / generated.name)
            text_reports = {}
            for report in gcdir.glob("*.gcov"):
                text = report.read_text()
                first = text.splitlines()[0]
                if ":Source:" in first:
                    text_reports[Path(first.split(":Source:", 1)[1]).name] = text
            for entry in payload["files"]:
                if entry["file"].startswith("<") and not entry["lines"] and not entry.get("functions"):
                    rec["source_omissions"].append({
                        "file": entry["file"], "object": note.relative_to(work).as_posix(),
                        "reason": "empty_compiler_virtual_source", "lines": 0, "branches": 0})
                    continue
                source = Path(entry["file"])
                if not source.is_absolute():
                    source = Path(payload["current_working_directory"]) / source
                source = source.resolve()
                if not source.is_relative_to(work.resolve()):
                    rec["source_omissions"].append({
                        "file": str(source), "object": note.relative_to(work).as_posix(),
                        "reason": "outside_generated_working_tree",
                        "lines": len(entry["lines"]),
                        "branches": sum(len(line.get("branches", [])) for line in entry["lines"])})
                    continue  # System headers are retained in raw gcov, outside report scope.
                if not source.is_file():
                    raise StageError("gcov", f"Missing source: {source}")
                relative = source.relative_to(work).as_posix()
                key = relative + "@" + note.relative_to(work).as_posix()
                identity = [unit["unit"], args.language, args.encodings, args.acn_v2,
                            args.slim, args.word_size, key]
                stat = analyse_file(entry, source, identity, args.language)
                stat["source"] = relative
                stat["component"] = file_component(source.name)
                text_report = source_text_report(entry, text_reports, source.name)
                if not text_report:
                    rec["source_omissions"].append({
                        "file": relative, "object": note.relative_to(work).as_posix(),
                        "reason": "empty_source_mapping_has_no_text_report", "lines": 0, "branches": 0})
                stat["text_gcov_misses"] = text_gcov_misses(text_report, args.language)
                if stat["official_line_misses"] != stat["text_gcov_misses"]:
                    raise StageError("gcov", f"JSON/text line-gate mismatch: {source.name}")
                stat["json_line_misses"] = stat["official_line_misses"]
                stat["official_line_misses"] = text_gcov_misses(
                    text_report, args.language, literal_legacy=True)
                stat["legacy_grep_false_positives"] = sorted(
                    set(stat["official_line_misses"]) - set(stat["json_line_misses"]))
                rec["files"][key] = stat
                observed.add(source.name)
        if not expected or expected - observed:
            raise StageError("gcov", f"Unmeasured generated codec bodies: {sorted(expected - observed)}")
    except StageError as error:
        rec.update(status="timeout" if error.timeout else error.stage.split("-")[0] + "_error",
                   failed_stage=error.stage, detail=error.detail)
    except (OSError, ValueError, KeyError, TypeError) as error:
        rec.update(status="collection_error", detail=str(error))
    rec["seconds"] = round(time.monotonic() - start, 3)
    write_json(directory / "unit.json", rec)
    return rec


def aggregate(records):
    components, groups, taxonomy = defaultdict(metrics), defaultdict(metrics), Counter()
    misses = {"enforced": 0, "exempt": 0}
    for rec in records:
        if rec["status"] != "ok":
            continue
        for stat in rec["files"].values():
            for key in METRICS:
                components[stat["component"]][key] += stat[key]
            if stat["component"] == "codec":
                taxonomy.update(stat["taxonomy"])
                misses["exempt" if rec["nocoverage"] else "enforced"] += len(stat["official_line_misses"])
                for group, counts in stat["groups"].items():
                    for key in METRICS:
                        groups[group][key] += counts[key]
    for counts in components.values():
        counts["legacy_adjusted_lines_total"] = counts["lines_total"] - counts["legacy_excluded_lines"]
        counts["legacy_adjusted_lines_hit"] = counts["lines_hit"] - counts["legacy_excluded_lines_hit"]
        counts["justified_branch_exclusions"] = 0
        counts["adjusted_branches_total"] = counts["branches_total"]
        counts["adjusted_branches_taken"] = counts["branches_taken"]
    return {"components": dict(components), "codec_function_groups": dict(groups),
            "codec_untaken_pattern_categories": dict(taxonomy), "legacy_line_misses": misses}


def compare_reference(records, reference):
    by_id = {r["unit"]: r for r in records}
    differences = []
    for uid, expected in reference["units"].items():
        rec = by_id.get(uid)
        if not rec or rec["status"] != "ok":
            differences.append({"unit": uid, "reason": "missing_or_failed"})
            continue
        if rec["input_sha256"] != expected["input_sha256"]:
            differences.append({"unit": uid, "reason": "input_changed"})
            continue
        counts = aggregate([rec])["components"].get("codec", metrics())
        for key in ("lines_total", "lines_hit", "branches_total", "branches_taken"):
            if counts[key] != expected[key]:
                differences.append({"unit": uid, "metric": key,
                                    "expected": expected[key], "actual": counts[key]})
    return {"reference_units": len(reference["units"]), "differences": differences,
            "matched": not differences}


def percent(hit, total):
    return f"{100 * hit / total:.2f}%" if total else "n/a"


def report(run_dir, summary):
    text = ["# Generated-code coverage", "",
            f"Run status: {summary['status']}. Measurement failures: {summary['failures']}.",
            "", "This report measures lines, basic blocks and branch arms, not source statements or MC/DC.",
            "Counts accumulate generated units; repeated uPER code and header instantiations are not deduplicated.",
            "", "| Scope | Raw lines | Raw branch arms | Basic blocks | Partial lines |",
            "|---|---|---|---|---|"]
    for name, stat in summary["measurements"]["components"].items():
        cells = [f"{stat[a]}/{stat[b]} ({percent(stat[a], stat[b])})"
                 for a, b in (("lines_hit", "lines_total"), ("branches_taken", "branches_total"),
                              ("blocks_hit", "blocks_total"))]
        text.append("| " + " | ".join([name, *cells, str(stat["partially_executed_lines"])]) + " |")
    text.extend(["", "## Legacy line adjustment (not reachability evidence)", "",
                 "| Scope | Adjusted lines | Excluded lines | Justified branch exclusions |",
                 "|---|---|---|---|"])
    for name, stat in summary["measurements"]["components"].items():
        hit, total = stat["legacy_adjusted_lines_hit"], stat["legacy_adjusted_lines_total"]
        text.append(f"| {name} | {hit}/{total} ({percent(hit, total)}) | "
                    f"{stat['legacy_excluded_lines']} | {stat['justified_branch_exclusions']} |")
    text.extend(["", "## Codec functions by role / encoding", "",
                 "| Role | Raw lines | Raw branch arms |", "|---|---|---|"])
    for name, stat in summary["measurements"]["codec_function_groups"].items():
        text.append(f"| {name} | {stat['lines_hit']}/{stat['lines_total']} | "
                    f"{stat['branches_taken']}/{stat['branches_total']} |")
    text.extend(["", "## Coverage policy", "",
                 "Legacy line exclusions are reporting conventions, not reachability proofs.",
                 "No branch exclusions are applied: adjusted branch coverage equals raw coverage.",
                 "Every untaken arm remains unresolved; each exact-source branch ID is in unit.json.",
                 "", "## Inventory and failures", "",
                 f"Selection reasons: {json.dumps(summary['inventory_counts'], sort_keys=True)}.",
                 f"Measured unit statuses: {json.dumps(summary['unit_statuses'], sort_keys=True)}.",
                 f"Legacy codec line misses: {json.dumps(summary['measurements']['legacy_line_misses'])}."])
    for error in summary["errors"]:
        text.append(f"- {error['unit']}: {error['status']} — {error.get('detail', '')[:300]}")
    if summary.get("baseline_comparison"):
        comparison = summary["baseline_comparison"]
        text.extend(["", "## Historical comparison", "",
                     f"Exact match: {comparison['matched']}; differences: {len(comparison['differences'])}."])
    text.extend(["", "See manifest.json, inventory.json, summary.json and units/*/ for commands, inputs, source and raw gcov.", ""])
    (run_dir / "report.md").write_text("\n".join(text))


def tool_version(command):
    result = subprocess.run(command, capture_output=True, text=True, timeout=30, check=True)
    return (result.stdout + result.stderr).strip()


def arguments(argv=None):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--outdir", type=Path, default=Path("coverage-results"))
    ap.add_argument("--compiler", type=Path, default=Path(os.environ.get(
        "ASN1SCC_COMPILER", str(V4.parent / "asn1scc/bin/Debug/net10.0/asn1scc"))))
    ap.add_argument("--test-root", type=Path, default=Path(os.environ.get(
        "ASN1SCC_TESTROOT", str(V4 / "test-cases/acn"))))
    ap.add_argument("--language", choices=("c", "Ada"), default="c")
    ap.add_argument("--encodings", choices=("both", "acn", "uper"), default="both")
    ap.add_argument("--acn-v2", action="store_true")
    ap.add_argument("--slim", action="store_true")
    ap.add_argument("--word-size", type=int, choices=(4, 8), default=8)
    ap.add_argument("--cohort", choices=("all", "historical", "pilot"), default="all")
    ap.add_argument("-t", "--filter")
    ap.add_argument("--limit", type=int)
    ap.add_argument("--jobs", type=int, default=2, help="Build jobs within one unit (units are sequential)")
    ap.add_argument("--timeout", type=int, default=600, help="Seconds per external stage")
    ap.add_argument("--gcc", default="gcc-13")
    ap.add_argument("--gcov", default="gcov-13")
    ap.add_argument("--inventory-only", action="store_true")
    ap.add_argument("--compare-baseline", action="store_true")
    ap.add_argument("--enforce-legacy-line-gate", action="store_true")
    ap.add_argument("--min-branch", type=float)
    ap.add_argument("--reference", type=Path, default=Path(os.environ.get(
        "ASN1SCC_COVERAGE_REFERENCE", str(V4 / "coverage/phase0-reference.json"))))
    ap.add_argument("--build-manifest", type=Path, default=Path(os.environ.get(
        "ASN1SCC_BUILD_MANIFEST", str(V4 / "coverage/build.json"))))
    ap.add_argument("--write-build-manifest", type=Path)
    ap.add_argument("--source-root", type=Path, default=V4.parent)
    ap.add_argument("--source-revision", default="unspecified")
    args = ap.parse_args(argv)
    if args.jobs < 1 or args.timeout < 1 or (args.limit is not None and args.limit < 1):
        ap.error("jobs, timeout and limit must be positive")
    if args.min_branch is not None and not 0 <= args.min_branch <= 100:
        ap.error("min-branch must be between 0 and 100")
    if args.acn_v2 and args.encodings == "uper":
        ap.error("--acn-v2 requires ACN encoding")
    if args.inventory_only and (args.compare_baseline or args.enforce_legacy_line_gate
                                or args.min_branch is not None):
        ap.error("Inventory-only cannot perform measurement or baseline gates")
    if args.language == "Ada" and args.word_size != 8:
        ap.error("The initial Ada measurement lane supports x86 word-size 8 only")
    if args.compare_baseline and (args.cohort not in ("historical", "all") or args.language != "c"
                                 or args.acn_v2 or args.slim or args.encodings != "both"
                                 or args.word_size != 8 or args.filter or args.limit):
        ap.error("Baseline comparison requires all historical IDs in a complete historical/all C/both/legacy/8-byte/non-slim run")
    return args


def main(argv=None):
    args = arguments(argv)
    if args.write_build_manifest:
        write_json(args.write_build_manifest, {
            "schema_version": SCHEMA, "source_revision": args.source_revision,
            "source": tree_manifest(args.source_root.resolve()),
            "dotnet": tool_version(["dotnet", "--info"]),
            "packages": tool_version(["dpkg-query", "-W"])})
        return 0
    run_dir = args.outdir.resolve() / (datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ-") + uuid.uuid4().hex[:8])
    run_dir.mkdir(parents=True, exist_ok=False)
    print(f"Report directory: {run_dir}", flush=True)
    manifest = {"schema_version": SCHEMA, "started_utc": datetime.now(timezone.utc).isoformat(),
                "configuration": {k: str(v) if isinstance(v, Path) else v for k, v in vars(args).items()},
                "compiler_flags": compiler_flags(args), "python": sys.version,
                "platform": platform.platform(), "uid": os.getuid(), "gid": os.getgid(),
                "collector_sha256": file_hash(Path(__file__)),
                "base_image": os.environ.get("ASN1SCC_COVERAGE_BASE_IMAGE", "not supplied"),
                "statement_coverage": {"measured": False, "reason": "gcov line/block/branch instrumentation"},
                "identity_scope": "exact generated-source hash, configuration, translation unit, function, line, branch ordinal; not semantic across source revisions"}
    records, inventory, errors = [], [], []
    exit_code = 0
    comparison = None
    try:
        args.test_root, args.compiler = args.test_root.resolve(), args.compiler.resolve()
        if not args.test_root.is_dir():
            raise ValueError("Test root does not exist")
        reference = json.loads(args.reference.read_text()) if (
            args.cohort == "historical" or args.compare_baseline) else None
        inventory = enumerate_units(args.test_root)
        if not inventory:
            raise ValueError("No behavior-0 directives found")
        selected = select_units(inventory, args, reference)
        if not selected:
            raise ValueError("Selection contains no runnable units")
        write_json(run_dir / "inventory.json", inventory)
        manifest["corpus_sha256"] = digest(json.dumps(
            [{k: u.get(k) for k in ("unit", "input_sha256", "input_error")} for u in inventory],
            sort_keys=True).encode())
        if args.inventory_only:
            print(f"{len(inventory)} inventoried, {len(selected)} selected; no compiler invoked")
        else:
            manifest["tool_versions"] = {"gcc": tool_version([args.gcc, "--version"]),
                                         "gcov": tool_version([args.gcov, "--version"])}
            if args.language == "Ada":
                manifest["tool_versions"]["gnat"] = tool_version(["gnatmake", "--version"])
                manifest["tool_versions"]["gprbuild"] = tool_version(["gprbuild", "--version"])
            if not args.compiler.is_file():
                raise ValueError(f"Compiler not found: {args.compiler}")
            manifest["compiler"] = tree_manifest(args.compiler.parent)
            if args.build_manifest.is_file():
                manifest["build"] = json.loads(args.build_manifest.read_text())
            else:
                manifest["build"] = {"source_revision": args.source_revision,
                                     "source_provenance": "not supplied; compiler bytes are hashed"}
            write_json(run_dir / "manifest.json", manifest)
            with (run_dir / "units.jsonl").open("w") as stream:
                for index, unit in enumerate(selected, 1):
                    rec = measure_unit(unit, args, run_dir)
                    records.append(rec)
                    stream.write(json.dumps(rec) + "\n")
                    stream.flush()
                    print(f"[{index}/{len(selected)}] {unit['unit']}: {rec['status']}", flush=True)
            if args.compare_baseline:
                comparison = compare_reference(records, reference)
                if not comparison["matched"]:
                    exit_code = 1
    except (OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
        errors.append({"unit": "run", "status": "setup_error", "detail": str(error)})
        exit_code = 1
    except KeyboardInterrupt:
        errors.append({"unit": "run", "status": "interrupted", "detail": "Run interrupted"})
        exit_code = 130
    errors.extend(r for r in records if r["status"] != "ok")
    measurements = aggregate(records)
    counts = measurements["components"].get("codec", metrics())
    gate_failure = args.enforce_legacy_line_gate and measurements["legacy_line_misses"]["enforced"] > 0
    if args.min_branch is not None:
        gate_failure |= not counts["branches_total"] or (
            100 * counts["branches_taken"] / counts["branches_total"] < args.min_branch)
    if errors or gate_failure:
        exit_code = exit_code or 1
    summary = {"schema_version": SCHEMA, "status": "failed" if exit_code else (
        "inventory_only" if args.inventory_only else "ok"),
        "failures": len(errors), "errors": errors, "threshold_failed": bool(gate_failure),
        "unit_statuses": dict(Counter(r["status"] for r in records)),
        "inventory_counts": dict(Counter(u.get("selection", "not_selected") for u in inventory)),
        "measurements": measurements, "baseline_comparison": comparison,
        "completed_utc": datetime.now(timezone.utc).isoformat()}
    write_json(run_dir / "manifest.json", manifest)
    write_json(run_dir / "summary.json", summary)
    if not (run_dir / "inventory.json").exists():
        write_json(run_dir / "inventory.json", inventory)
    report(run_dir, summary)
    print(f"Status: {summary['status']}; failures: {summary['failures']}; report: {run_dir / 'report.md'}")
    return exit_code


if __name__ == "__main__":
    sys.exit(main())
