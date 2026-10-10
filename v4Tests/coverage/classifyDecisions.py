#!/usr/bin/env python3
"""Classify the uncovered decision and MC/DC obligations of statementCollector.py runs.

Reads measurement directories made with --level stmt+mcdc (each with units.jsonl
and units/<id>/work/ holding the generated sources). Every decision of a codec
file that misses an outcome in the stmt+decision report, and every condition
without MC/DC in the stmt+mcdc report, becomes one row, labelled with:
  - the enclosing generated function and its kind (classifyStatements.function_kind);
  - the template construct, a source pattern of the decision (see `construct`);
  - what is missing: outcome TRUE, outcome FALSE, never evaluated, or the
    independence pair of one condition;
  - single-condition decision (decision coverage = MC/DC) or compound;
  - `proven`: the ID of a documented exemption (Docs/statement-coverage-exemptions.md)
    when its precondition, checked on the source, proves the missing outcome
    impossible.
Labels other than `proven` are source patterns only, not reachability claims.
"""
import argparse
from collections import Counter
import csv
import json
from pathlib import Path
import re
import sys

sys.path.insert(0, str(Path(__file__).resolve().parent))
from classifyStatements import (CHAR_GUARD, configuration, decoded_range, enclosing,  # noqa: E402
                                function_kind, function_starts, table)

CONSTRUCTS = [
    # (label, pattern on the decision's source line); the first match wins.
    ("charIndex guard", re.compile(r"\(\s*charIndex\s*<\s*0")),
    ("errcode select", re.compile(r"\?\s*0\s*:|\(if\s+\w+(\.Success)?\s+then\s+0\s+else\b", re.I)),
    ("ret select", re.compile(r"\bret\s*\?")),
    ("ret and no error code", re.compile(r"\bret\s*&&\s*\*pErrCode\s*==\s*0"
                                        r"|\bresult\.success\s+and\s+(then\s+)?result\.ErrorCode\s*=\s*0", re.I)),
    ("alignment", re.compile(r"\bmod\s+8\b|%\s*8\b")),
    ("validity call", re.compile(r"IsConstraintValid|bCheckConstraints|_IsValid\b|Is_Valid\b", re.I)),
    ("ret check", re.compile(r"^\s*(\}\s*)?(else\s+)?(if|while)\s*\(?\s*(not\s+)?\(?\s*(ret|result)(\.Success)?\s*\)?\s*(\)|then|loop)", re.I)),
    ("loop with ret", re.compile(r"^\s*(for|while)\b.*\b(ret|result\.Success)\b", re.I)),
    ("presence / CHOICE selection", re.compile(r"\bexist\b|\.Exist\.|present|\.kind\b|_selection\b|PRESENT", re.I)),
    ("size / length", re.compile(r"nCount|\.Length\b|\bnSize|Length\s*\(|GetLength|nLength|_length\b|Last\b", re.I)),
    ("loop", re.compile(r"^\s*(for|while)\b", re.I)),
    ("range check", re.compile(r"(<=|>=|<|>)")),
    ("equality", re.compile(r"==|/=|!=|\s=\s")),
]


def construct(src):
    for label, pattern in CONSTRUCTS:
        if pattern.search(src):
            return label
    return "other"


def missing_outcome(decision):
    if decision["status"] == "-":
        return "never evaluated"
    found = {m.split()[1] for m in decision["messages"] if m.startswith("outcome ")}
    return {"TRUE": "TRUE", "FALSE": "FALSE"}.get("".join(sorted(found)), "+".join(sorted(found)) or "?")


def decision_proven(language, lines, first, missing):
    """ASN1SCC-COV-002, decision form: the charIndex guard right after
    BitStream_DecodeConstraintWholeNumber(.., &charIndex, 0, N) can never be TRUE,
    so neither its TRUE outcome nor the independence pairs of its range conditions
    can be exercised."""
    if language != "c" or missing == "FALSE":
        return ""
    match = CHAR_GUARD.search(lines[first - 1])
    if match and decoded_range(lines, first, "charIndex") == (0, int(match[1])):
        return "ASN1SCC-COV-002"
    return ""


def track(kind, label, missing):
    """Candidate track of plan §4 for a missing outcome (a hint, not a claim)."""
    if label == "charIndex guard":
        return "T4"
    if kind == "equal":
        return "T1 (unequal pairs)"
    if kind == "validate":
        return "T2 (invalid values)"
    if label in {"errcode select", "ret select", "ret check", "ret and no error code", "loop with ret"} \
            and missing == "FALSE":
        return "T1/T3 (failing decode)" if kind.endswith("decode") else "T4/T5 (encode cannot fail)"
    if label == "validity call":
        return "T2 (invalid values)"
    return "?"


def config_name(run):
    """classifyStatements.configuration, with '/pusc' for the PUS-C cohort."""
    name, language = configuration(run)
    config = json.loads((run / "manifest.json").read_text())["configuration"]
    return (name.replace("/acn", "") + "/pusc" if config.get("pusc_units") else name), language


def classify(run):
    name, language = config_name(run)
    decision_rows, mcdc_rows, units = [], [], []
    for record in map(json.loads, (run / "units.jsonl").read_text().splitlines()):
        work = run / record["directory"] / "work"
        exempt = bool(record.get("legacy_line_gate_exempt"))
        entry = {"config": name, "unit": record["unit"], "status": record["status"], "exempt": exempt,
                 "decisions": 0, "decisions_covered": 0, "conditions": 0, "conditions_covered": 0}
        for file_name, data in record.get("files", {}).items():
            if data["component"] != "codec" or "decision" not in data:
                continue
            for key, prefix in (("decision", "decisions"), ("mcdc", "conditions")):
                if key in data:
                    entry[prefix] += data[key]["counts"]["total"]
                    entry[prefix + "_covered"] += data[key]["counts"]["covered"]
            lines = (work / file_name).read_text(errors="replace").splitlines()
            starts = function_starts(lines, "c" if language == "c" else "Ada")

            def base(decision):
                first = int(decision["spans"][0]["num"])
                src = lines[first - 1].strip()
                function = enclosing(starts, first)
                return first, {"config": name, "unit": record["unit"], "exempt": exempt, "file": file_name,
                               "line": first, "function": function, "function_kind": function_kind(function),
                               "construct": construct(src),
                               "shape": "single" if len(decision["conditions"]) <= 1 else "compound",
                               "conditions": len(decision["conditions"]), "src": src[:200]}

            for decision in data["decision"]["decisions"]:
                if decision["status"] not in {"!", "-"}:
                    continue
                first, row = base(decision)
                row["missing"] = missing_outcome(decision)
                row["proven"] = decision_proven(language, lines, first, row["missing"])
                row["track"] = track(row["function_kind"], row["construct"], row["missing"])
                decision_rows.append(row)
            for decision in data.get("mcdc", {}).get("decisions", []):
                if decision["status"] == ".":
                    continue
                for condition in decision["conditions"]:
                    if condition["status"] == "+":
                        continue
                    first, row = base(decision)
                    row["condition"] = condition["text"][:80]
                    row["missing"] = ("independence pair" if condition["status"] == "-" else
                                      "decision outcome" if decision["status"] in {"-", "!"} or
                                      decision["messages"] else "not evaluated")
                    row["proven"] = decision_proven(language, lines, first, "TRUE")
                    mcdc_rows.append(row)
        units.append(entry)
    return name, decision_rows, mcdc_rows, units


def write_csv(path, rows):
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]) if rows else ["config"])
        writer.writeheader()
        writer.writerows(rows)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("runs", nargs="+", type=Path, help="measurement directories (with units.jsonl)")
    ap.add_argument("--outdir", type=Path, required=True)
    ap.add_argument("--top", type=int, default=25)
    args = ap.parse_args(argv)
    args.outdir.mkdir(parents=True, exist_ok=False)
    decisions, conditions, units, configs = [], [], [], []
    for run in args.runs:
        name, d, m, u = classify(run)
        configs.append(name)
        decisions += d
        conditions += m
        units += u
    write_csv(args.outdir / "uncovered-decisions.csv", decisions)
    write_csv(args.outdir / "uncovered-conditions.csv", conditions)
    write_csv(args.outdir / "units.csv", units)

    report = ["# Uncovered generated-code decisions and MC/DC conditions", "",
              "Decision = control-flow decision of the `stmt+decision` report (both outcomes needed).",
              "MC/DC = condition of the `stmt+mcdc` report (independent influence needed; this report",
              "also counts Boolean expressions outside control flow). Source-pattern labels only,",
              "except `proven` (Docs/statement-coverage-exemptions.md, checked per decision).", "",
              "| configuration | scope | units ok | decisions covered / total | proven | covered or proven "
              "| MC/DC conditions covered / total | proven | covered or proven |",
              "|---|---|---:|---:|---:|---:|---:|---:|---:|"]
    summary = {}
    for name in configs:
        for scope, keep in (("all", lambda u: True), ("non-exempt", lambda u: not u["exempt"]),
                            ("exempt", lambda u: u["exempt"])):
            ok = [u for u in units if u["config"] == name and keep(u) and u["status"] == "ok"]
            names = {u["unit"] for u in ok}
            values = {k: sum(u[k] for u in ok) for k in
                      ("decisions", "decisions_covered", "conditions", "conditions_covered")}
            values["decisions_proven"] = sum(1 for r in decisions if r["config"] == name and r["proven"]
                                             and r["unit"] in names and keep(r))
            values["conditions_proven"] = sum(1 for r in conditions if r["config"] == name and r["proven"]
                                              and r["unit"] in names and keep(r))
            values["units_ok"] = len(ok)
            summary[f"{name}|{scope}"] = values
            pct = lambda n, t: f"{100 * n / t if t else 0:.1f}%"
            d, dc, dp = values["decisions"], values["decisions_covered"], values["decisions_proven"]
            m, mc, mp = values["conditions"], values["conditions_covered"], values["conditions_proven"]
            report.append(f"| {name} | {scope} | {len(ok)} | {dc} / {d} ({pct(dc, d)}) | {dp} | {pct(dc + dp, d)} "
                          f"| {mc} / {m} ({pct(mc, m)}) | {mp} | {pct(mc + mp, m)} |")
    labels = [label for label, _ in CONSTRUCTS] + ["other"]
    for name in configs:
        mine = [r for r in decisions if r["config"] == name and not r["proven"]]
        report += ["", f"## {name}: unproven missing decision outcomes, construct x missing outcome", "",
                   table(Counter((r["construct"], r["missing"]) for r in mine), labels,
                         ["FALSE", "TRUE", "never evaluated"], "construct", "missing")]
        fkinds = sorted({r["function_kind"] for r in mine})
        report += ["", f"## {name}: unproven missing decision outcomes, function kind x construct", "",
                   table(Counter((r["function_kind"], r["construct"]) for r in mine), fkinds, labels,
                         "function kind", "construct")]
        report += ["", f"## {name}: candidate track of the unproven missing decision outcomes", "",
                   "| track | decisions |", "|---|---:|"]
        report += [f"| {t} | {n} |" for t, n in Counter(r["track"] for r in mine).most_common()]
        cmine = [r for r in conditions if r["config"] == name and not r["proven"]]
        report += ["", f"## {name}: unproven MC/DC conditions, decision shape x reason", "",
                   table(Counter((r["shape"], r["missing"]) for r in cmine), ["single", "compound"],
                         ["decision outcome", "independence pair", "not evaluated"], "decision shape", "reason")]
    patterns = Counter((r["function_kind"], r["construct"], r["missing"],
                        re.sub(r"\b[A-Z][A-Za-z0-9_]*_([A-Za-z0-9_]+)\b", "<ID>", r["src"])[:90])
                       for r in decisions if not r["proven"])
    report += ["", f"## Top {args.top} decision patterns (all configurations, identifiers masked)", "",
               "| function kind | construct | missing | pattern | count |", "|---|---|---|---|---:|"]
    report += [f"| {f} | {c} | {m} | `{p.replace('|', '/')}` | {n} |"
               for (f, c, m, p), n in patterns.most_common(args.top)]
    compound = Counter((r["function_kind"], r["construct"],
                        re.sub(r"\b[A-Z][A-Za-z0-9_]*_([A-Za-z0-9_]+)\b", "<ID>", r["src"])[:90])
                       for r in conditions if r["shape"] == "compound" and r["missing"] == "independence pair"
                       and not r["proven"])
    report += ["", f"## Top {args.top} compound decisions without MC/DC (independence pair missing)", "",
               "| function kind | construct | pattern | conditions |", "|---|---|---|---:|"]
    report += [f"| {f} | {c} | `{p.replace('|', '/')}` | {n} |" for (f, c, p), n in compound.most_common(args.top)]
    (args.outdir / "report.md").write_text("\n".join(report) + "\n")
    (args.outdir / "summary.json").write_text(json.dumps({"configurations": summary}, indent=2,
                                                         sort_keys=True) + "\n")
    print(args.outdir / "report.md")
    return 0


if __name__ == "__main__":
    sys.exit(main())
