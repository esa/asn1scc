#!/usr/bin/env python3
"""Plan of the parallel CI regression (.github/workflows/asn1scc-tests-parallel.yml).

The single source of the jobs, the regression legs and their shards:

  ciPlan.py --matrix               JSON list of job names (the matrix of the test jobs)
  ciPlan.py --expected OUT.tsv     the test cases every regression leg must run (regression --list)
  ciPlan.py --run JOB              run the steps of one job; every step runs, exit 1 if one failed
  ciPlan.py --summary RESULTS EXPECTED [--trx DIR]
                                   markdown report (stdout): per leg expected / ran / ok / failed /
                                   missing cases, the failures, the slowest cases; exit 1 if a case failed
                                   or is missing

Run from the repository root, inside the asn1scc image (except --summary and --matrix, plain python3).
A regression step writes ci-results/<job>-<n>.json (regression --json); the summary checks that the
shards of each leg together ran exactly the cases of the unsharded leg.
"""

import argparse
import collections
import glob
import json
import os
import subprocess
import sys
import xml.etree.ElementTree as ET

REGRESSION = "../regression/bin/Debug/net10.0/regression"
RESULTS_DIR = "ci-results"
PARALLEL = 4   # GitHub-hosted runners of public repositories: 4 vCPU


def reg(leg, spark=None, shard=None):
    """A regression step: leg = regression arguments selecting one configuration (as in local-build.sh)."""
    return {"kind": "regression", "leg": leg, "spark": spark, "shard": shard}


def sh(cmd, cwd="v4Tests"):
    return {"kind": "shell", "cmd": cmd, "cwd": cwd}


ADA_SPARK_SHARDS = 4


def ada_ws8(leg):
    return [[reg(leg, spark="exclude")]] + \
           [[reg(leg, spark="only", shard=f"{k}/{ADA_SPARK_SHARDS}")] for k in range(1, ADA_SPARK_SHARDS + 1)]


def scala(test_filter):
    return [sh("dotnet build Antlr/ && dotnet build parseStg2/ && dotnet build asn1scc.sln", cwd="."),
            sh(f'dotnet test --no-build --filter "{test_filter}" '
               f'--logger "trx;LogFilePrefix=scala" --results-directory ../{RESULTS_DIR}/trx', cwd="PUSCInteropTest")]


def build_jobs():
    jobs = collections.OrderedDict()
    jobs["ada-rtl-x86"] = [sh("./scripts/proveAdaRtl.sh x86")]
    jobs["ada-rtl-msp430"] = [sh("./scripts/proveAdaRtl.sh msp430")]
    jobs["ada-ws4"] = [reg("-l Ada -ws 4 -s false")]
    jobs["ada-ws4-acnv2"] = [reg("-l Ada -ws 4 -s false -acnv2")]
    for suffix, leg in [("", "-l Ada -ws 8 -s false"), ("-acnv2", "-l Ada -ws 8 -s false -acnv2")]:
        steps = ada_ws8(leg)
        jobs[f"ada-ws8{suffix}-nospark"] = steps[0]
        for k, s in enumerate(steps[1:], 1):
            jobs[f"ada-ws8{suffix}-spark-{k}"] = s
    jobs["c-ws4"] = [reg("-l c -ws 4 -s false"), reg("-l c -ws 4 -s false -acnv2")]
    jobs["c-ws8-slim"] = [reg("-l c -ws 8 -s true -ig"), reg("-l c -ws 8 -s true")]
    jobs["c-ws8"] = [reg("-l c -ws 8 -s false"), reg("-l c -ws 8 -s true -acnv2")]
    jobs["c-ws8-acnv2"] = [reg("-l c -ws 8 -s false -acnv2"),
                           sh("./scripts/runWireTests.sh"),
                           sh("./scripts/runIcdPdusTests.sh"),
                           sh("python3 ./scripts/runIcdTests.py")]
    jobs["python"] = [reg("-l python -ws 4 -s false"), reg("-l python -ws 8 -s false"),
                      sh("uvx --python=3.11 pytest tests -v", cwd="asn1python"),
                      sh("./tools/check_generated_code.sh", cwd="asn1python")]
    jobs["rust-ws4"] = [reg("-l Rust -ws 4 -s false")]
    jobs["rust-ws8"] = [reg("-l Rust -ws 8 -s false")]
    jobs["scala-encdec-c-python"] = scala("FullyQualifiedName~PUS_C_Interop_Test.EncDecC|FullyQualifiedName~PUS_C_Interop_Test.EncDecPython")
    jobs["scala-encdec-scala"] = scala("FullyQualifiedName~PUS_C_Interop_Test.EncDecScala")
    jobs["scala-interop-acn"] = scala("FullyQualifiedName~PUS_C_Interop_Test.InteropAcn")
    jobs["scala-interop-uper-xer"] = scala("FullyQualifiedName~PUS_C_Interop_Test.InteropUper|FullyQualifiedName~PUS_C_Interop_Test.InteropXer")
    return jobs


SCALA_CLASSES = ["EncDecC", "EncDecPython", "EncDecScala", "InteropAcn", "InteropUper", "InteropXer"]


def legs(jobs):
    """The distinct regression configurations, in plan order."""
    seen = []
    for steps in jobs.values():
        for s in steps:
            if s["kind"] == "regression" and s["leg"] not in seen:
                seen.append(s["leg"])
    return seen


def regression_cmd(step, out_json):
    cmd = f"{REGRESSION} {step['leg']} -p {PARALLEL}"
    if step["spark"]:
        cmd += f" --spark {step['spark']}"
    if step["shard"]:
        cmd += f" --shard {step['shard']}"
    return cmd + f" --json {out_json}"


def run_job(name, jobs):
    root = os.getcwd()
    os.makedirs(RESULTS_DIR, exist_ok=True)
    failed = []
    for n, step in enumerate(jobs[name], 1):
        if step["kind"] == "regression":
            cwd = "v4Tests"
            cmd = regression_cmd(step, os.path.join(root, RESULTS_DIR, f"{name}-{n}.json"))
        else:
            cwd, cmd = step["cwd"], step["cmd"]
        print(f"=== [{name} {n}/{len(jobs[name])}] ({cwd}) {cmd}", flush=True)
        rc = subprocess.call(cmd, shell=True, cwd=os.path.join(root, cwd), executable="/bin/bash")
        print(f"=== [{name} {n}/{len(jobs[name])}] exit code {rc}", flush=True)
        if rc != 0:
            failed.append(f"step {n}: {cmd} (exit {rc})")
    with open(os.path.join(RESULTS_DIR, f"{name}.steps.json"), "w") as f:
        json.dump({"job": name, "failed": failed}, f)
    for line in failed:
        print(f"FAILED {line}")
    return 1 if failed else 0


def write_expected(out, jobs):
    with open(out, "w") as f:
        for leg in legs(jobs):
            listing = subprocess.run(f"{REGRESSION} {leg} --list", shell=True, cwd="v4Tests",
                                     executable="/bin/bash", check=True, capture_output=True, text=True).stdout
            for line in listing.splitlines():
                if line.strip():
                    f.write(line + "\n")
    return 0


def leg_key(entry):
    return f"l={entry['lang']} ws={entry['ws']} slim={str(entry['slim']).lower()} " \
           f"ig={str(entry['ig']).lower()} acnv2={str(entry['acnv2']).lower()}"


def summary(results_dir, expected_file, trx_dir):
    expected = collections.defaultdict(set)
    with open(expected_file) as f:
        for line in f:
            parts = line.rstrip("\n").split("\t")
            if len(parts) < 6:
                continue
            cid, rest = parts[0], dict(p.split("=", 1) for p in parts[1:])
            key = f"l={rest['l']} ws={rest['ws']} slim={rest['slim']} ig={rest['ig']} acnv2={rest['acnv2']}"
            expected[key].add(cid)

    ran = collections.defaultdict(list)
    for fn in sorted(glob.glob(os.path.join(results_dir, "**", "*.json"), recursive=True)):
        if fn.endswith(".steps.json"):
            continue
        with open(fn) as f:
            for e in json.load(f):
                ran[leg_key(e)].append(e)

    step_failures = []
    for fn in sorted(glob.glob(os.path.join(results_dir, "**", "*.steps.json"), recursive=True)):
        with open(fn) as f:
            d = json.load(f)
        step_failures += [f"{d['job']}: {x}" for x in d["failed"]]

    bad = False
    out = ["## Regression legs", "",
           "| leg | expected | ran | OK | failed | missing | duplicated | case-seconds |",
           "|---|---:|---:|---:|---:|---:|---:|---:|"]
    failures, all_entries = [], []
    for key in sorted(set(expected) | set(ran)):
        entries = ran.get(key, [])
        ids = [e["id"] for e in entries]
        dup = len(ids) - len(set(ids))
        missing = expected.get(key, set()) - set(ids)
        extra = set(ids) - expected.get(key, set())
        nok = sum(e["ok"] for e in entries)
        nfail = len(entries) - nok
        secs = sum(e["seconds"] for e in entries)
        if nfail or missing or dup or extra:
            bad = True
        out.append(f"| {key} | {len(expected.get(key, ()))} | {len(entries)} | {nok} | {nfail} | "
                   f"{len(missing)} | {dup} | {secs:.0f} |")
        failures += [(key, e) for e in entries if not e["ok"]]
        failures += [(key, {"id": m, "message": "MISSING: no shard ran this case", "workDir": ""}) for m in sorted(missing)]
        failures += [(key, {"id": x, "message": "UNEXPECTED: not in the unsharded list", "workDir": ""}) for x in sorted(extra)]
        all_entries += [(key, e) for e in entries]
    total_ok = sum(1 for _, e in all_entries if e["ok"])
    out += ["", f"Total: {len(all_entries)} cases run, {total_ok} OK "
                f"(the `--> OK` lines of the sequential flow)."]

    if trx_dir and os.path.isdir(trx_dir):
        ns = {"t": "http://microsoft.com/schemas/VisualStudio/TeamTest/2010"}
        per_class = collections.defaultdict(lambda: collections.Counter())
        for fn in glob.glob(os.path.join(trx_dir, "**", "*.trx"), recursive=True):
            root = ET.parse(fn).getroot()
            names = {}
            for ut in root.iter(f"{{{ns['t']}}}UnitTest"):
                tm = ut.find("t:TestMethod", ns)
                names[ut.get("id")] = tm.get("className").split(".")[-1] if tm is not None else "?"
            for r in root.iter(f"{{{ns['t']}}}UnitTestResult"):
                cls = names.get(r.get("testId"), "?")
                per_class[cls][r.get("outcome")] += 1
                h, m, s = r.get("duration", "0:0:0").split(":")
                per_class[cls]["seconds"] += int(h) * 3600 + int(m) * 60 + float(s)
        out += ["", "## PUSCInteropTest (dotnet test)", "",
                "| class | passed | failed | skipped | seconds |", "|---|---:|---:|---:|---:|"]
        for cls in sorted(set(per_class) | set(SCALA_CLASSES)):
            c = per_class.get(cls, collections.Counter())
            if not c:
                bad = True
            if c["Failed"]:
                bad = True
            out.append(f"| {cls} | {c['Passed']} | {c['Failed']} | {c['NotExecuted']} | {c['seconds']:.0f} |")
        tot = collections.Counter()
        for c in per_class.values():
            tot.update(c)
        out.append(f"| **total** | {tot['Passed']} | {tot['Failed']} | {tot['NotExecuted']} | {tot['seconds']:.0f} |")

    if step_failures:
        bad = True
        out += ["", "## Failed steps", ""] + [f"- `{x}`" for x in step_failures]
    if failures:
        out += ["", f"## Failed / missing cases ({len(failures)})", ""]
        for key, e in failures[:200]:
            msg = (e.get("message") or "").strip().splitlines()
            out.append(f"- **{e['id']}** ({key}){' wd=' + e['workDir'] if e.get('workDir') else ''}: "
                       f"`{(msg[0] if msg else '')[:300]}`")
    slow = sorted(all_entries, key=lambda ke: -ke[1]["seconds"])[:15]
    if slow:
        out += ["", "## Slowest cases", "", "| case | leg | seconds |", "|---|---|---:|"]
        out += [f"| {e['id']} | {key} | {e['seconds']} |" for key, e in slow]
    print("\n".join(out))
    return 1 if bad else 0


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument("--matrix", action="store_true")
    g.add_argument("--expected", metavar="OUT")
    g.add_argument("--run", metavar="JOB")
    g.add_argument("--summary", nargs=2, metavar=("RESULTS", "EXPECTED"))
    ap.add_argument("--trx", metavar="DIR")
    a = ap.parse_args()
    jobs = build_jobs()
    if a.matrix:
        print(json.dumps(list(jobs)))
        return 0
    if a.expected:
        return write_expected(a.expected, jobs)
    if a.run:
        if a.run not in jobs:
            sys.exit(f"unknown job {a.run}; jobs: {', '.join(jobs)}")
        return run_job(a.run, jobs)
    return summary(a.summary[0], a.summary[1], a.trx)


if __name__ == "__main__":
    sys.exit(main())
