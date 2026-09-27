#!/usr/bin/env python3
"""Classify the uncovered statement obligations of statementCollector.py runs.

Reads one or more exported measurement directories (each with units.jsonl and
units/<id>/work/ holding the generated sources), finds the generated function
that encloses every codec statement obligation and groups the obligations by
function kind and statement kind. The labels are source patterns only: they
say where a missed statement is, not whether a test can reach it.
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
    if direction and "xer" in fn:
        return "xer-" + direction
    if direction and "ber" in fn:
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
                             "statement_kind": statement_kind(src, lines, first, last), "src": src[:200]})
        units.append(entry)
    return name, rows, units


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
              "Source-pattern labels only; no reachability claim.", "",
              "`exempt` = units with a NOCOVERAGE directive (exempt from the legacy line gate).", "",
              "| configuration | units | units ok / measured | statements covered / total | uncovered |",
              "|---|---|---:|---:|---:|"]
    summary = {}
    for name in configs:
        for scope, keep in (("all", lambda u: True), ("non-exempt", lambda u: not u["exempt"]),
                            ("exempt", lambda u: u["exempt"])):
            mine = [u for u in units if u["config"] == name and keep(u)]
            ok = [u for u in mine if u["status"] == "ok"]
            total, covered = sum(u["total"] for u in ok), sum(u["covered"] for u in ok)
            summary[f"{name}|{scope}"] = {"units": len(mine), "units_ok": len(ok),
                                          "total": total, "covered": covered}
            report.append(f"| {name} | {scope} | {len(ok)} / {len(mine)} | {covered} / {total} "
                          f"({100 * covered / total if total else 0:.1f}%) | {total - covered} |")
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
    return 0


if __name__ == "__main__":
    sys.exit(main())
