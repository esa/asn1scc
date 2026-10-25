#!/usr/bin/env python3
"""Plan of the parallel CI regression (.github/workflows/asn1scc-tests.yml).

The single source of the jobs, the regression legs and their shards:

  ciPlan.py --matrix               JSON list of job names (the matrix of the test jobs)
  ciPlan.py --expected OUT.tsv     the test cases every regression leg must run (regression --list)
  ciPlan.py --run JOB              run the steps of one job; every step runs, exit 1 if one failed
  ciPlan.py --summary RESULTS EXPECTED [--trx DIR]
                                   markdown report (stdout): per leg expected / ran / ok / failed /
                                   missing cases, the PUSCInteropTest methods, the failures, the slowest
                                   cases; exit 1 if a case / test failed or is missing
  ciPlan.py --update-timings RESULTS
                                   rewrite v4Tests/scripts/ci-timings.json from the results of a run
                                   (the regression --json files and the PUSCInteropTest .trx files)

Run from the repository root, inside the asn1scc image (except --summary and --matrix, plain python3).
A regression step writes ci-results/<job>-<n>.json (regression --json); the summary checks that the
shards of each leg together ran exactly the cases of the unsharded leg.

Shards are balanced by the measured durations in ci-timings.json (regression --timings: longest
processing time first; PUSCInteropTest: by PUS service, so that a shard builds each service once).
Refresh the file after a run whose shards came out uneven: download the results-* artifacts, run
--update-timings, commit.
"""

import argparse
import collections
import glob
import json
import os
import re
import subprocess
import sys
import xml.etree.ElementTree as ET

REGRESSION = "../regression/bin/Debug/net10.0/regression"
RESULTS_DIR = "ci-results"
PARALLEL = 4   # GitHub-hosted runners of public repositories: 4 vCPU
HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(HERE))
TIMINGS = os.path.join(HERE, "ci-timings.json")
PUSC_DIR = os.path.join(REPO, "PUSCInteropTest")
PUSC_KEY = "PUSCInteropTest"     # key of the per-service durations in ci-timings.json


def reg(leg, spark=None, shard=None):
    """A regression step: leg = regression arguments selecting one configuration (as in local-build.sh)."""
    return {"kind": "regression", "leg": leg, "spark": spark, "shard": shard}


def sh(cmd, cwd="v4Tests"):
    return {"kind": "shell", "cmd": cmd, "cwd": cwd}


def load_timings():
    if os.path.exists(TIMINGS):
        with open(TIMINGS) as f:
            return json.load(f)
    return {}


def pusc_methods():
    """(class, method) of every [TestMethod] (also the ignored ones) in the PUSCInteropTest sources."""
    out = []
    for fn in sorted(glob.glob(os.path.join(PUSC_DIR, "*", "*.cs"))):
        text = open(fn, encoding="utf-8-sig").read()
        cls = re.search(r"public class (\w+)", text)
        for m in re.finditer(r"\[TestMethod[^\]]*\]\s*public void (\w+)\(", text):
            out.append((cls.group(1), m.group(1)))
    return out


def service_of(method):
    return re.match(r"S\d+", method).group(0)


def lpt(weights, n):
    """Longest processing time first: {item: weight} -> n lists of items."""
    bins = [[] for _ in range(n)]
    loads = [0.0] * n
    for item, w in sorted(weights.items(), key=lambda kv: (-kv[1], kv[0])):
        j = loads.index(min(loads))
        bins[j].append(item)
        loads[j] += w
    return bins


def scala_shards(n):
    """PUSCInteropTest split by PUS service: every test of a service (all classes) in the same shard, so that
    the shard generates and builds that service once (BuildCache)."""
    methods = pusc_methods()
    services = sorted({service_of(m) for _, m in methods}, key=lambda x: int(x[1:]))
    known = load_timings().get(PUSC_KEY, {})
    default = sorted(known.values())[len(known) // 2] if known else 1.0
    bins = lpt({svc: known.get(svc, default) for svc in services}, n)
    shards = []
    for b in bins:
        names = sorted({m for _, m in methods if service_of(m) in b})
        shards.append("|".join(f"Name={x}" for x in names))
    return shards


def scala(test_filter):
    return [sh("dotnet build Antlr/ && dotnet build parseStg2/ && dotnet build asn1scc.sln", cwd="."),
            sh(f'dotnet test --no-build --filter "{test_filter}" '
               f'--logger "trx;LogFilePrefix=scala" --results-directory ../{RESULTS_DIR}/trx', cwd="PUSCInteropTest")]


def sharded(legs_, k, n):
    return [reg(leg, shard=f"{k}/{n}") for leg in legs_]


ADA_WS8_SHARDS = 3
C_SHARDS = 4
SCALA_SHARDS = 3
C_LEGS = ["-l c -ws 4 -s false", "-l c -ws 4 -s false -acnv2",
          "-l c -ws 8 -s true -ig", "-l c -ws 8 -s true", "-l c -ws 8 -s false",
          "-l c -ws 8 -s true -acnv2", "-l c -ws 8 -s false -acnv2"]


def build_jobs():
    jobs = collections.OrderedDict()
    jobs["ada-rtl-x86"] = [sh("./scripts/proveAdaRtl.sh x86")]
    jobs["ada-rtl-msp430"] = [sh("./scripts/proveAdaRtl.sh msp430")]
    jobs["ada-ws4"] = [reg("-l Ada -ws 4 -s false"), reg("-l Ada -ws 4 -s false -acnv2")]
    for suffix, leg in [("", "-l Ada -ws 8 -s false"), ("-acnv2", "-l Ada -ws 8 -s false -acnv2")]:
        for k in range(1, ADA_WS8_SHARDS + 1):
            jobs[f"ada-ws8{suffix}-{k}"] = [reg(leg, shard=f"{k}/{ADA_WS8_SHARDS}")]
    # every C leg split over the C jobs; the script checks go to the first two
    for k in range(1, C_SHARDS + 1):
        jobs[f"c-{k}"] = sharded(C_LEGS, k, C_SHARDS)
    jobs["c-1"] += [sh("./scripts/runWireTests.sh"), sh("./scripts/runIcdPdusTests.sh")]
    jobs["c-2"] += [sh("python3 ./scripts/runIcdTests.py")]
    jobs["python"] = [reg("-l python -ws 4 -s false"), reg("-l python -ws 8 -s false"),
                      sh("uvx --python=3.11 pytest tests -v", cwd="asn1python"),
                      sh("./tools/check_generated_code.sh", cwd="asn1python")]
    jobs["rust"] = [reg("-l Rust -ws 4 -s false"), reg("-l Rust -ws 8 -s false")]
    for k, f in enumerate(scala_shards(SCALA_SHARDS), 1):
        jobs[f"scala-{k}"] = scala(f)
    return jobs


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
    if os.path.exists(TIMINGS):
        cmd += f" --timings {TIMINGS}"
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

    if trx_dir is not None:
        expected_methods = collections.Counter(pusc_methods())
        seen = collections.Counter()
        per_class = collections.defaultdict(collections.Counter)
        for cls, name, outcome, secs in trx_results(trx_dir):
            seen[(cls, name)] += 1
            per_class[cls][outcome] += 1
            per_class[cls]["seconds"] += secs
        missing = sorted(set(expected_methods) - set(seen))
        dups = sorted(k for k, v in seen.items() if v > 1)
        failed_tests = sum(c["Failed"] for c in per_class.values())
        if missing or dups or failed_tests:
            bad = True
        out += ["", "## PUSCInteropTest (dotnet test)", "",
                "| class | passed | failed | skipped | seconds |", "|---|---:|---:|---:|---:|"]
        tot = collections.Counter()
        for cls in sorted({c for c, _ in expected_methods} | set(per_class)):
            c = per_class.get(cls, collections.Counter())
            tot.update(c)
            out.append(f"| {cls} | {c['Passed']} | {c['Failed']} | {c['NotExecuted']} | {c['seconds']:.0f} |")
        out.append(f"| **total** | {tot['Passed']} | {tot['Failed']} | {tot['NotExecuted']} | {tot['seconds']:.0f} |")
        out.append("")
        out.append(f"Test methods in the sources: {sum(expected_methods.values())}, in the results: "
                   f"{sum(seen.values())}, missing: {len(missing)}, duplicated: {len(dups)}.")
        out += [f"- MISSING {c}.{m}" for c, m in missing[:50]] + [f"- DUPLICATED {c}.{m}" for c, m in dups[:50]]

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


def trx_results(trx_dir):
    """(class, method, outcome, seconds) of every test result in the .trx files under trx_dir."""
    ns = {"t": "http://microsoft.com/schemas/VisualStudio/TeamTest/2010"}
    for fn in glob.glob(os.path.join(trx_dir, "**", "*.trx"), recursive=True):
        root = ET.parse(fn).getroot()
        names = {}
        for ut in root.iter(f"{{{ns['t']}}}UnitTest"):
            tm = ut.find("t:TestMethod", ns)
            names[ut.get("id")] = (tm.get("className").split(".")[-1], tm.get("name"))
        for r in root.iter(f"{{{ns['t']}}}UnitTestResult"):
            cls, name = names[r.get("testId")]
            h, m, sec = r.get("duration", "0:0:0").split(":")
            yield cls, name, r.get("outcome"), int(h) * 3600 + int(m) * 60 + float(sec)


def update_timings(results_dir):
    timings = collections.defaultdict(dict)
    for fn in sorted(glob.glob(os.path.join(results_dir, "**", "*.json"), recursive=True)):
        if fn.endswith(".steps.json"):
            continue
        with open(fn) as f:
            for e in json.load(f):
                timings[leg_key(e)][e["id"]] = e["seconds"]
    per_service = collections.Counter()
    for _, name, _, secs in trx_results(results_dir):
        per_service[service_of(name)] += secs
    if per_service:
        timings[PUSC_KEY] = {k: round(v, 1) for k, v in per_service.items()}
    out = {k: dict(sorted(v.items())) for k, v in sorted(timings.items())}
    with open(TIMINGS, "w") as f:
        json.dump(out, f, indent=0, sort_keys=True)
        f.write("\n")
    print(f"{TIMINGS}: {sum(len(v) for v in out.values())} entries")
    return 0


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument("--matrix", action="store_true")
    g.add_argument("--expected", metavar="OUT")
    g.add_argument("--run", metavar="JOB")
    g.add_argument("--summary", nargs=2, metavar=("RESULTS", "EXPECTED"))
    g.add_argument("--update-timings", metavar="RESULTS")
    ap.add_argument("--trx", metavar="DIR")
    a = ap.parse_args()
    jobs = build_jobs()
    if a.matrix:
        print(json.dumps(list(jobs)))
        return 0
    if a.update_timings:
        return update_timings(a.update_timings)
    if a.expected:
        return write_expected(a.expected, jobs)
    if a.run:
        if a.run not in jobs:
            sys.exit(f"unknown job {a.run}; jobs: {', '.join(jobs)}")
        return run_job(a.run, jobs)
    return summary(a.summary[0], a.summary[1], a.trx)


if __name__ == "__main__":
    sys.exit(main())
