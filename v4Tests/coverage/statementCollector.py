"""Measure generated-code statement obligations with GNATcoverage source traces.

With --level stmt+decision, stmt+mcdc or stmt+uc_mcdc, one instrumented build
also yields the decision (branch) and MC/DC obligations: GNATcoverage writes one
report per level from the same source trace (a stmt+uc_mcdc trace also gives the
masking stmt+mcdc report), and every level is accounted strictly.
"""
import argparse
from collections import Counter
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import uuid
import xml.etree.ElementTree as ET

from encodePilot import c


def statement_metrics(element):
    stats = [] if element is None else element.findall("obligation_stats[@kind='Stmt']")
    if len(stats) != 1:
        raise ValueError("Missing statement obligation statistics")
    metrics = stats[0].findall("metric")
    counts = {m.attrib["kind"]: int(m.attrib["count"]) for m in metrics}
    if len(counts) != len(metrics):
        raise ValueError("Duplicate statement metric")
    total = counts["total_obligations_of_relevance"]
    covered, missed = counts["fully_covered"], counts["not_covered"]
    if any(v < 0 for v in counts.values()) or total != covered + missed:
        raise ValueError("Undetermined, partial, exempted or disabled statement obligations")
    if any(v for k, v in counts.items()
           if k not in {"total_obligations_of_relevance", "fully_covered", "not_covered"}):
        raise ValueError("Unsupported statement obligation status")
    return {"statements_total": total, "statements_covered": covered,
            "statements_uncovered": missed}


LEVELS = ("stmt", "stmt+decision", "stmt+mcdc", "stmt+uc_mcdc")


def report_levels(level):
    """Report levels produced from a trace instrumented at `level`."""
    return LEVELS[:LEVELS.index(level) + 1]


def report_dir_name(level):
    return "report" if level == "stmt" else "report-" + level


def obligation_counts(element, kind):
    """Strict summary counts of one obligation kind (Decision, MCDC)."""
    stats = [] if element is None else element.findall(f"obligation_stats[@kind='{kind}']")
    if len(stats) != 1:
        raise ValueError(f"Missing {kind} obligation statistics")
    counts = {m.attrib["kind"]: int(m.attrib["count"]) for m in stats[0].findall("metric")}
    known = {"total_obligations_of_relevance", "fully_covered", "partially_covered", "not_covered"}
    if any(v for k, v in counts.items() if k not in known) or any(v < 0 for v in counts.values()):
        raise ValueError(f"Undetermined, exempted or disabled {kind} obligations")
    result = {"total": counts["total_obligations_of_relevance"], "covered": counts["fully_covered"],
              "partial": counts["partially_covered"], "uncovered": counts["not_covered"]}
    if result["total"] != result["covered"] + result["partial"] + result["uncovered"]:
        raise ValueError(f"Inconsistent {kind} obligation statistics")
    return result


def spans_of(node):
    return [dict(line.attrib) for line in node.findall("src/line")]


def read_decisions(report_dir, work, level, names):
    """Decision obligations (stmt+decision) or MC/DC condition obligations (stmt+mcdc,
    stmt+uc_mcdc).

    Decision report: a decision with status +, ! (one outcome missing) or - (never
    evaluated) is an obligation; '.' marks a Boolean expression that is a decision
    only at the MC/DC level. MC/DC report: every condition of a decision is one
    obligation, covered only with status '+'. Unique-cause MC/DC (stmt+uc_mcdc) counts
    only the conditions it could assess: a '.' condition (its decision lacks an
    outcome) is left out there, while masking MC/DC counts it as not covered.
    Counts are cross-checked per file and
    globally against GNATcoverage's own summary.
    """
    kind = {"stmt+decision": "Decision", "stmt+mcdc": "MCDC", "stmt+uc_mcdc": "UC_MCDC"}[level]
    index = ET.parse(report_dir / "xml/index.xml").getroot().find("coverage_report")
    if index is None or index.get("coverage_level") != level:
        raise ValueError(f"Expected a GNATcoverage {level} report")
    summary = index.find("coverage_summary")
    files = {}
    for include in index.findall("sources/{http://www.w3.org/2001/XInclude}include"):
        path = (report_dir / "xml" / include.attrib["href"]).resolve()
        if not path.is_relative_to((report_dir / "xml").resolve()):
            raise ValueError("Report include escapes its directory")
        source = ET.parse(path).getroot()
        if source.tag != "source" or source.get("coverage_level") != level:
            raise ValueError("Invalid source report")
        name = Path(source.attrib["file"]).resolve().relative_to(work.resolve()).as_posix()
        if name not in names:
            raise ValueError(f"Source missing from the statement report: {name}")
        messages = {}
        for message in source.iter("message"):
            sco = re.match(r"SCO #(\d+)", message.get("SCO", ""))
            if sco and message.get("kind") == "violation":
                messages.setdefault(sco[1], []).append(message.attrib["message"])
        decisions, ids = [], set()
        for node in source.iter("decision"):
            sco, status = node.attrib["id"], node.attrib["coverage"]
            if sco in ids:
                raise ValueError("Duplicate source decision identity")
            ids.add(sco)
            if status not in {"+", "!", "-", "."}:
                raise ValueError(f"Unresolved decision status {status} in {name}")
            conditions = []
            for condition in node.findall("condition"):
                if condition.attrib["coverage"] not in {"+", "-", "!", "."}:
                    raise ValueError(f"Unresolved condition status in {name}")
                conditions.append({"sco_id": condition.attrib["id"], "status": condition.attrib["coverage"],
                                   "text": condition.get("text", ""), "spans": spans_of(condition),
                                   "messages": messages.get(condition.attrib["id"], [])})
            decisions.append({"sco_id": sco, "status": status, "text": node.get("text", ""),
                              "spans": spans_of(node), "conditions": conditions,
                              "messages": messages.get(sco, [])})
        relevant = [d for d in decisions if d["status"] != "."]
        if kind == "Decision":
            counts = {"total": len(relevant), "covered": sum(d["status"] == "+" for d in relevant),
                      "partial": sum(d["status"] == "!" for d in relevant),
                      "uncovered": sum(d["status"] == "-" for d in relevant)}
        else:
            conditions = [x for d in relevant for x in d["conditions"]
                          if kind == "MCDC" or x["status"] != "."]
            covered = sum(x["status"] == "+" for x in conditions)
            counts = {"total": len(conditions), "covered": covered, "partial": 0,
                      "uncovered": len(conditions) - covered}
        files[name] = {"counts": counts, "decisions": decisions}
    indexed = {}
    for element in summary.findall("file"):
        name = Path(element.attrib["name"]).resolve().relative_to(work.resolve()).as_posix()
        indexed[name] = obligation_counts(element, kind)
    if indexed.keys() != files.keys():
        raise ValueError("Source inventory differs from report summary")
    for name, counts in indexed.items():
        if counts != files[name]["counts"]:
            raise ValueError(f"{kind} accounting differs for {name}")
    if obligation_counts(summary, kind) != {k: sum(f["counts"][k] for f in files.values())
                                            for k in ("total", "covered", "partial", "uncovered")}:
        raise ValueError(f"{kind} accounting differs from global report")
    return files


def read_report(report_dir, work):
    """Count statements, cross-check XML summary, and retain each obligation."""
    index = ET.parse(report_dir / "xml/index.xml").getroot().find("coverage_report")
    if index is None or index.get("coverage_level") != "stmt":
        raise ValueError("Expected a GNATcoverage statement report")
    summary = index.find("coverage_summary")
    expected = statement_metrics(summary)
    files = {}
    for include in index.findall("sources/{http://www.w3.org/2001/XInclude}include"):
        path = (report_dir / "xml" / include.attrib["href"]).resolve()
        if not path.is_relative_to((report_dir / "xml").resolve()):
            raise ValueError("Report include escapes its directory")
        source = ET.parse(path).getroot()
        if source.tag != "source" or source.get("coverage_level") != "stmt":
            raise ValueError("Invalid source report")
        original = Path(source.attrib["file"]).resolve()
        if not original.is_relative_to(work.resolve()) or not original.is_file():
            raise ValueError(f"Unmapped source: {original}")
        name = original.relative_to(work).as_posix()
        if name in files:
            raise ValueError(f"Duplicate source report: {name}")
        sha = c.file_hash(original)
        obligations, non_obligations, ids = [], [], set()
        occurrences = Counter()
        for node in source.findall(".//statement"):
            sco = node.attrib["id"]
            if sco in ids:
                raise ValueError("Duplicate source statement identity")
            ids.add(sco)
            status = node.attrib["coverage"]
            spans = [dict(line.attrib) for line in node.findall("src/line")]
            entry = {"sco_id": sco, "status": status, "spans": spans,
                     "text": node.attrib.get("text", "")}
            coordinate = json.dumps([name, sha, spans, entry["text"]], sort_keys=True)
            entry["id"] = c.digest(json.dumps([coordinate, occurrences[coordinate]]).encode())
            occurrences[coordinate] += 1
            if status == ".":
                non_obligations.append(entry)
            elif status in {"+", "-"}:
                obligations.append(entry)
            else:
                raise ValueError(f"Unresolved instrumentation status {status} in {name}")
        counts = {"statements_total": len(obligations),
                  "statements_covered": sum(o["status"] == "+" for o in obligations),
                  "statements_uncovered": sum(o["status"] == "-" for o in obligations)}
        files[name] = {"source_sha256": sha, "component": c.file_component(original.name),
                       **counts, "obligations": obligations,
                       "non_obligation_nodes": non_obligations}
    indexed = {}
    for element in summary.findall("file"):
        name = Path(element.attrib["name"]).resolve().relative_to(work.resolve()).as_posix()
        if name in indexed:
            raise ValueError("Duplicate summary source")
        indexed[name] = statement_metrics(element)
    if indexed.keys() != files.keys():
        raise ValueError("Source inventory differs from report summary")
    for name, counts in indexed.items():
        if counts != {k: files[name][k] for k in counts}:
            raise ValueError(f"Statement accounting differs for {name}")
    if expected != {k: sum(f[k] for f in files.values()) for k in expected}:
        raise ValueError("Statement accounting differs from global report")
    return files


def instrument_and_run(work, project, units, args, logs, steps):
    gnatcov = os.environ.get("GNATCOV", "gnatcov")
    common = ["-P" + project, *["--units=" + unit for unit in units]]
    c.run_step([gnatcov, "instrument", *common, "--level=" + args.level,
                "--restricted-to-languages=" + ("C" if args.language == "c" else "Ada"),
                "--dump-trigger=atexit", "--dump-channel=bin-file"],
               work, logs, "instrument", args.timeout, steps)
    build = ["gprbuild", "-p", "-P" + project, "-j" + str(args.jobs),
             "--src-subdirs=gnatcov-instr", "--implicit-with=gnatcov_rts"]
    if args.language == "Ada":
        # GNATcoverage's documented SPARK instrumentation configuration affects
        # proof-only pragmas, not runtime checks or Pre/Post assertion policy.
        build.extend(["-cargs:Ada", "-gnatec=" + str(work / "instrument-spark.adc")])
    c.run_step(build, work, logs, "build", args.timeout, steps)
    output = c.run_step([str(work / "mainprogram")], work, logs, "run", c.runtime_timeout(args),
                        steps, strict_stderr=True)
    match = re.search(r"All test cases \((\d+)\) run successfully", output)
    if not match or int(match[1]) == 0:
        raise c.StageError("run", "No positive automatic tests executed")
    traces = sorted(work.glob("*.srctrace"))
    sids = sorted(work.rglob("*.sid"))
    if len(traces) != 1 or not sids or not traces[0].stat().st_size:
        raise c.StageError("instrument", "Missing or unexpected source trace/SID inventory")
    for level in report_levels(args.level):
        stage = "coverage" if level == "stmt" else "coverage-" + level
        c.run_step([gnatcov, "coverage", *common, "--level=" + level, "--annotate=xml,xcov,report",
                    "--output-dir=" + report_dir_name(level), str(traces[0])],
                   work, logs, stage, args.timeout, steps)
    artifacts = {p.relative_to(work).as_posix(): c.file_hash(p) for p in traces + sids}
    return int(match[1]), artifacts


def measure(unit, args, root):
    directory = root / "units" / c.digest(unit["unit"].encode())[:16]
    work, logs = directory / "work", directory / "logs"
    work.mkdir(parents=True)
    logs.mkdir()
    record = {"unit": unit["unit"], "input_sha256": unit.get("input_sha256"),
              "legacy_line_gate_exempt": unit["nocoverage"],
              "status": "ok", "steps": [], "files": {}, "directory": str(directory.relative_to(root))}
    try:
        if "input_error" in unit:
            raise c.StageError("setup", unit["input_error"])
        if "pusc_files" in unit:
            compile_pusc(unit, args, work, logs, record["steps"])
        else:
            c.prepare_work(unit, args.test_root, work)
            c.run_step([str(args.compiler), *c.compiler_flags(args), "-o", str(work),
                        "sample1.asn1", "sample1.acn"], work, logs, "compile", args.timeout,
                       record["steps"], strict_stderr="warnings")
        if args.language == "c":
            record["decode_checks"] = {"stage": "baseline", "prefix_checks": 0}
            if args.decode_stage != "baseline":
                record["decode_checks"] = c.decode_support().prepare(work, unit["unit"], args)
            if args.check_invalid_values:
                record["invalid_value_checks"] = c.harness_support("invalidValueHarness").prepare(work, unit["unit"], args)
            if args.check_invalid_streams:
                record["invalid_stream_checks"] = c.harness_support("invalidStreamHarness").prepare(work, unit["unit"], args)
        suffix = ".c" if args.language == "c" else ".adb"
        # Files of <name>.helpers/ are hand-written (mapping functions, checks), not generated code.
        helpers = set() if "pusc_files" in unit else {name for name, _ in c.helper_files(unit, args.test_root)}
        bodies = sorted(p.name for p in work.glob("*" + suffix)
                        if c.file_component(p.name) == "codec" and p.name not in helpers)
        # Instrument C encode/decode helpers too, to verify that the optional
        # second encode actually executes. Their metrics remain separate.
        harness = sorted(p.name for p in work.glob("*_auto_tcs.c")) if args.language == "c" else []
        harness += sorted(h for h in helpers if h.endswith(".c")) if args.language == "c" else []
        units = bodies + harness if args.language == "c" else [Path(p).stem for p in bodies]
        if not units:
            raise ValueError("No generated codec units")
        if args.check_encode and not any("#ifdef ASN1SCC_CHECK_ENCODE" in p.read_text()
                                         for p in work.glob("*_auto_tcs.c")):
            raise ValueError("Generated harness does not support checked/unchecked encoding")
        if args.language == "c":
            flags = ["-g", "-O0", "-Wall", "-Wextra", "-Werror", "-D_DEBUG"]
            if args.check_encode:
                flags.append("-DASN1SCC_CHECK_ENCODE")
            if args.decode_stage != "baseline":
                flags.append("-DASN1SCC_DECODE_ACTUAL_LENGTH")
            language, main = "C", "mainprogram.c"
        else:
            flags = ["-gnat2012", "-g", "-O0", "-gnatf", "-gnatwa", "-fstack-check"]
            language, main = "Ada", "mainprogram.adb"
            (work / "instrument-spark.adc").write_text("\n".join(
                "pragma Ignore_Pragma (" + pragma + ");" for pragma in
                ("SPARK_Mode", "Refined_State", "Abstract_State", "Global", "Depends",
                 "Part_Of", "Initializes", "Refined_Global", "Refined_Depends")) + "\n")
        quoted = lambda values: ", ".join('"' + x.replace('"', '""') + '"' for x in values)
        (work / "statement.gpr").write_text(
            'project Statement is\n for Languages use ("' + language + '");\n'
            ' for Source_Dirs use (".");\n for Object_Dir use "stmt-obj";\n'
            ' for Exec_Dir use ".";\n for Main use ("' + main + '");\n'
            ' package Compiler is\n'
            + (' for Driver ("C") use "gcc-13";\n' if args.language == "c" else '')
            + ' for Default_Switches ("' + language + '") use (' + quoted(flags) + ');\n'
            ' end Compiler;\n package Linker is\n for Default_Switches ("' + language + '") use ('
            # gprbuild links non-main objects through an archive, so a helper that
            # nothing references (a check run by a constructor before main) is left
            # out; link it explicitly. Referenced helpers come from the archive.
            + quoted(["-lm", *[str(work / "stmt-obj" / (Path(h).stem + ".o")) for h in sorted(helpers)
                               if h.endswith(".c") and "__attribute__((constructor))" in (work / h).read_text()]])
            + ');\n'
            ' end Linker;\nend Statement;\n')
        record["positive_tests"], record["artifacts_sha256"] = instrument_and_run(
            work, "statement.gpr", units, args, logs, record["steps"])
        if args.decode_stage != "baseline":
            c.decode_support().verify_output((logs / "run.stdout").read_text(), record["decode_checks"])
        if args.check_invalid_values:
            c.harness_support("invalidValueHarness").verify_output((logs / "run.stdout").read_text(), record["invalid_value_checks"])
        if args.check_invalid_streams:
            c.harness_support("invalidStreamHarness").verify_output((logs / "run.stdout").read_text(), record["invalid_stream_checks"])
        record["files"] = read_report(work / "report", work)
        for name in helpers & record["files"].keys():
            record["files"][name]["component"] = "harness"
        for level in report_levels(args.level)[1:]:
            key = {"stmt+decision": "decision", "stmt+mcdc": "mcdc", "stmt+uc_mcdc": "uc_mcdc"}[level]
            for name, data in read_decisions(work / report_dir_name(level), work, level,
                                             record["files"].keys()).items():
                record["files"][name][key] = data
        namespace = [unit["unit"], args.language, args.encodings, args.acn_v2, args.slim, args.word_size]
        for file in record["files"].values():
            for obligation in file["obligations"] + file["non_obligation_nodes"]:
                obligation["id"] = c.digest(json.dumps([namespace, obligation["id"]]).encode())
        for name, file in record["files"].items():
            for key in ("decision", "mcdc", "uc_mcdc"):
                occurrences = Counter()
                for decision in file.get(key, {}).get("decisions", []):
                    coordinate = json.dumps([name, file["source_sha256"], decision["spans"], decision["text"]],
                                            sort_keys=True)
                    decision["id"] = c.digest(json.dumps([namespace, coordinate, occurrences[coordinate]]).encode())
                    occurrences[coordinate] += 1
        if set(bodies) - record["files"].keys():
            raise ValueError("Missing generated codec source in statement report")
        if not sum(f["statements_total"] for f in record["files"].values() if f["component"] == "codec"):
            raise ValueError("No measured generated-code statement obligations")
        if args.check_encode:
            checks = [o for name in harness for o in record["files"].get(name, {}).get("obligations", [])
                      if any("&uncheckedStrm, &uncheckedError" in span.get("src", "") for span in o["spans"])]
            if not checks or any(o["status"] != "+" for o in checks):
                raise ValueError("Unchecked encode call sites were not all executed")
            record["covered_unchecked_call_sites"] = len(checks)
        if args.decode_stage != "baseline":
            checks = [o for name in harness for o in record["files"].get(name, {}).get("obligations", [])
                      if any("BitStream_AttachBuffer(&bitStrm, encBuff, (int)encLenBytes)" in span.get("src", "")
                             for span in o["spans"])]
            if not checks or any(o["status"] != "+" for o in checks):
                raise ValueError("Actual-length attach call sites were not all executed")
            record["covered_actual_length_call_sites"] = len(checks)
    except (c.StageError, OSError, ValueError, KeyError, ET.ParseError) as error:
        record.update(status="failed", detail=str(error),
                      failed_stage=error.stage if isinstance(error, c.StageError) else "collection")
    c.write_json(directory / "unit.json", record)
    return record


def compile_pusc(unit, args, work, logs, steps):
    """Compile one PUS-C service group (ASN.1 files with their optional ACN files).

    The N7 Space files emit multi-line warnings, which the line-based warning check
    of the corpus would reject; here any stderr line mentioning an error fails.
    """
    if args.encodings != "acn":
        raise ValueError("PUS-C units are measured with --encodings acn")
    root = args.pusc_root
    sources = [root / (f + ".asn1") for f in unit["pusc_files"]]
    sources += [root / (f + ".acn") for f in unit["pusc_files"] if (root / (f + ".acn")).is_file()]
    c.run_step([str(args.compiler), *c.compiler_flags(args), "-o", str(work), *map(str, sources)],
               work, logs, "compile", args.timeout, steps)
    if re.search(r"error", (logs / "compile.stderr").read_text(errors="replace"), re.IGNORECASE):
        raise c.StageError("compile", "Compiler reported an error")


def pusc_units(args):
    """Inventory of the PUS-C cohort: one unit per service group of the manifest."""
    groups = json.loads(args.pusc_units.read_text())["groups"]
    units = []
    for group, names in groups.items():
        unit = {"unit": "pusc/" + group, "pusc_files": names, "nocoverage": False, "no_atc": False}
        missing = [n for n in names if not (args.pusc_root / (n + ".asn1")).is_file()]
        if missing:
            unit["input_error"] = "Missing PUS-C files: " + ", ".join(missing)
        else:
            unit["input_sha256"] = c.digest(json.dumps(
                [[n, c.file_hash(args.pusc_root / (n + ext))] for n in names for ext in (".asn1", ".acn")
                 if (args.pusc_root / (n + ext)).is_file()]).encode())
        units.append(unit)
    for unit in units:
        unit["selection"] = "filter" if args.filter and args.filter not in unit["unit"] else "selected"
    selected = [u for u in units if u["selection"] == "selected"][:args.limit or None]
    for unit in units:
        if unit["selection"] == "selected" and unit not in selected:
            unit["selection"] = "limit"
    return units, selected


def split_arguments(argv):
    """Options of this collector; the rest goes to the shared collector parser."""
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument("--level", choices=LEVELS, default="stmt")
    parser.add_argument("--pusc-units", type=Path, help="PUS-C cohort: service-group manifest (JSON)")
    parser.add_argument("--pusc-root", type=Path, default=Path(os.environ.get(
        "ASN1SCC_PUSC_ROOT", "/opt/test-cases/pusc")))
    own, rest = parser.parse_known_args(sys.argv[1:] if argv is None else argv)
    args = c.arguments(rest)
    args.level, args.pusc_units, args.pusc_root = own.level, own.pusc_units, own.pusc_root.resolve()
    return args


def main(argv=None):
    args = split_arguments(argv)
    if (args.compare_baseline or args.enforce_legacy_line_gate or args.min_branch is not None
            or args.from_run or args.write_build_manifest or args.word_size != 8 or args.gcc != "gcc-13"):
        raise SystemExit("Statement lane requires the default native toolchain; gcov gates/exports do not apply")
    args.compiler, args.test_root = args.compiler.resolve(), args.test_root.resolve()
    root = args.outdir.resolve() / (datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ-") + uuid.uuid4().hex[:8])
    root.mkdir(parents=True, exist_ok=False)
    manifest = {"metric": "statement", "instrumentation": "GNATcoverage source traces --level=" + args.level,
                "configuration": {k: str(v) if isinstance(v, Path) else v for k, v in vars(args).items()},
                "compiler_flags": c.compiler_flags(args), "uid": os.getuid(), "gid": os.getgid(),
                "collector_sha256": c.file_hash(Path(__file__)),
                "scope": "generated codecs/helpers; raw statement obligations, no exclusions"}
    records, errors, inventory = [], [], []
    try:
        reference = json.loads(args.reference.read_text()) if args.cohort == "historical" else None
        if args.pusc_units:
            inventory, selected = pusc_units(args)
        else:
            inventory = c.enumerate_units(args.test_root)
            selected = c.select_units(inventory, args, reference)
        if not selected:
            raise ValueError("No runnable units selected")
        if not args.inventory_only:
            for name, command in {"gnatcov": [os.environ.get("GNATCOV", "gnatcov"), "--version"],
                                  "gcc": ["gcc-13", "--version"], "gnat": ["gnatmake", "--version"],
                                  "gprbuild": ["gprbuild", "--version"]}.items():
                manifest.setdefault("tools", {})[name] = c.tool_version(command)
            manifest["compiler"] = c.tree_manifest(args.compiler.parent)
            if args.build_manifest.is_file():
                manifest["compiler_build"] = json.loads(args.build_manifest.read_text())
            toolchain = Path('/opt/gnatcov/toolchain-build.json')
            if toolchain.is_file():
                manifest["gnatcov_build"] = json.loads(toolchain.read_text())
            with (root / "units.jsonl").open("w") as stream:
                for unit in selected:
                    record = measure(unit, args, root)
                    records.append(record)
                    stream.write(json.dumps(record) + "\n")
                    stream.flush()
                    print(unit["unit"], record["status"], record.get("detail", ""), flush=True)
    except (OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
        errors.append(str(error))
    errors.extend(r["unit"] + ": " + r["detail"] for r in records if r["status"] != "ok")
    totals = {k: sum(f[k] for r in records if r["status"] == "ok" for f in r["files"].values()
                     if f["component"] == "codec") for k in ("statements_total", "statements_covered", "statements_uncovered")}
    summary = {"status": "failed" if errors else ("inventory_only" if args.inventory_only else "ok"),
               "metric": "statement", "errors": errors, "unit_statuses": dict(Counter(r["status"] for r in records)),
               "statement_coverage_measured": any(r["status"] == "ok" for r in records),
               "inventory_counts": dict(Counter(u.get("selection", "not_selected") for u in inventory)),
               "codec": totals}
    for key in ("decision", "mcdc", "uc_mcdc")[:len(report_levels(args.level)) - 1]:
        summary["codec_" + key] = {k: sum(f[key]["counts"][k] for r in records if r["status"] == "ok"
                                          for f in r["files"].values() if f["component"] == "codec")
                                   for k in ("total", "covered", "partial", "uncovered")}
    for name, data in [("manifest", manifest), ("inventory", inventory), ("summary", summary)]:
        c.write_json(root / (name + ".json"), data)
    body = ('Inventory only; no execution or coverage measurement.\n' if args.inventory_only else
        f'GNATcoverage `--level=stmt`: **{totals["statements_covered"]}/{totals["statements_total"]}** '
        'generated-code statement obligations covered in successfully measured units. No exclusions applied.\n'
        + ''.join(f'\nGNATcoverage `--level={level}`: **{summary["codec_" + key]["covered"]}/'
                  f'{summary["codec_" + key]["total"]}** {key} obligations covered '
                  f'({summary["codec_" + key]["partial"]} partially).\n'
                  for level, key in zip(report_levels(args.level)[1:], ("decision", "mcdc", "uc_mcdc"))))
    (root / "report.md").write_text(
        '# Generated-code statement coverage\n\nStatus: ' + summary["status"] + '\n\n'
        + body + '\nCounts accumulate generated instances in the selected configurations.\n\n'
        + '\n'.join('- ' + error for error in errors) + '\n')
    print(f"Report: {root}", flush=True)
    return int(bool(errors))


if __name__ == "__main__":
    raise SystemExit(main())
