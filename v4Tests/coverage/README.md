# Reliable generated-code coverage (Phase 0b)

This lane builds asn1scc and measures its generated C/Ada code inside a Linux
container. Docker is the only required host tool. The final image runs as UID
10001 / GID 10001; source compilation also runs as that non-root user. No host
.NET SDK, Python, GCC, GNAT, gcov, or source-tree write mount is required.

## Build and run

From the repository root:

    docker build -f v4Tests/coverage/Dockerfile -t asn1scc-coverage:phase0b .

The base SDK image is pinned by digest. Package revisions and the exact source
tree are recorded during the build; compiler bytes and tool versions are
recorded at runtime. Preserve the built image (and its Docker image ID) when
comparing runs. Rebuilding later may obtain newer Ubuntu package revisions.
An optional build argument SOURCE_REVISION records a known commit SHA; the
source-content hash is always recorded, including uncommitted supplied changes.
The Dockerfile-specific ignore file excludes host build output and local q/tmp
notes from the context. The precompiled ANTLR dependency DLLs remain inputs.

Run without network access. A named container retains reports even on failure:

    docker run --name coverage-inventory --network none asn1scc-coverage:phase0b --outdir /results --inventory-only
    docker cp coverage-inventory:/results ./coverage-inventory

Do not add --rm if you intend to copy results afterwards. Use a new container
name per run. Every invocation creates a new report subdirectory and never
overwrites another run. After copying results you can remove that specific
finished container with docker rm.

Reproduce all original 259 units and compare individual counts and input hashes:

    docker run --name coverage-historical --network none asn1scc-coverage:phase0b --outdir /results --cohort historical --compare-baseline
    docker cp coverage-historical:/results ./coverage-historical

Measure all runnable behavior-0 directives, including NOCOVERAGE:

    docker run --name coverage-all --network none asn1scc-coverage:phase0b --outdir /results
    docker cp coverage-all:/results ./coverage-all

Add --compare-baseline to an all-cohort run to compare the original 259 IDs
within the expanded measurement, without rerunning that cohort separately.

The bounded pilot selects 12 directives covering integer, enum, strings/arrays,
CHOICE, shared determinants, OPTIONAL, parameter passing, CONTAINING and deduced
size. Run each mode separately so differences are visible:

    docker run --name coverage-c-pilot --network none asn1scc-coverage:phase0b --outdir /results --cohort pilot
    docker run --name coverage-c-v2 --network none asn1scc-coverage:phase0b --outdir /results --cohort pilot --acn-v2
    docker run --name coverage-ada-pilot --network none asn1scc-coverage:phase0b --outdir /results --cohort pilot --language Ada
    docker run --name coverage-ada-v2 --network none asn1scc-coverage:phase0b --outdir /results --cohort pilot --language Ada --acn-v2

Use docker cp on each named container to export results. --filter accepts a
unit/path substring; --limit caps selected units; --jobs controls compilation
parallelism within a unit; --timeout caps each external stage. --slim and
--encodings acn/uper/both make the measured configuration explicit. C supports
--word-size 4 or 8; the initial Ada lane uses x86/8. A uPER-only run cannot use
--acn-v2. Selecting no runnable units is an error, not a successful empty run.

## Reports and metric semantics

- manifest.json: configuration, UTC time, non-root identity, exact commands'
  compiler flags, source/build metadata, compiler hashes and tool versions.
- inventory.json: every behavior-0 directive with source line, input hash,
  gate exemption and selection/omission reason. NOCOVERAGE is measured;
  NO_AUTOMATIC_TEST_CASES remains explicitly outside runtime coverage.
- summary.json and report.md: successful measurements and failures separately.
  Totals are split into generated codec, RTL and harness; codec functions are
  additionally grouped as ACN, uPER, validation, equality, initialization,
  or other. Function-name grouping is heuristic; the original names remain.
- units.jsonl and units/*/unit.json: individual metrics, full source lines,
  every branch's ordinal/count and exact-source identity, exclusions and steps.
- units/*/work, logs and gcov: generated inputs/sources, instrumented build
  artifacts, stage stdout/stderr, textual gcov and original compressed JSON.
  Full sweeps can use substantial disk space; retain the complete report until
  reviewed rather than silently discarding evidence.

Raw lines, partially executed lines, function basic blocks and branch arms are
different metrics. The legacy line adjustment removes marked/trivia lines; it
is not a justification of unreachability. No branch exclusions are applied.
Every untaken branch remains unresolved, even if tagged B6/B8 or COVERAGE_IGNORE.
The B1-B8 taxonomy is preserved only for historical source-pattern comparisons.
The literal legacy grep is retained separately from the actual gcov count
column: hashes in decorative comments can produce legacy false positives.
Empty compiler virtual sources such as GNAT's <unknown> are inventoried with
zero counts; a nonempty unmappable source remains an error.
Translation units proved empty by gcov (no functions or executable lines)
legitimately have no .gcda. They are recorded explicitly with their .gcno hash
and zero counts. Missing runtime data for any object containing code still fails.

Branch IDs are stable for the same unit/configuration, generated-source hash,
translation unit, function, line and branch ordinal. They are not semantic
field/goal IDs across code changes. The schema reserves semantic_goal_id and
proof for later compiler metadata. Coverage accumulates generated instances:
uPER code repeated across ACN directives and headers instantiated in multiple
objects are not claimed as unique repository coverage.

Statement coverage is explicitly NOT measured by this gcov lane. For a future
source-statement claim the selected separate instrumentation lane is
GNATcoverage source instrumentation at --level=stmt for C/Ada, with its own
tool/version and generated-code compatibility validation. This image does not
provide gnatcov or claim statement/MC/DC evidence. Its line/block/branch data
provides the diagnostic baseline needed to target missing behavior.
See the [GNATcoverage instrumentation guide](https://docs.adacore.com/gnatcoverage-docs/html/src_traces.html).

## Failure and gate behavior

Any input/compiler/build/runtime/gcov/collection error or timeout makes the
run exit nonzero. Each failed unit remains failed and is excluded from
successful coverage totals; its diagnostics remain available. Timeouts terminate
the stage's process group. Missing or malformed profiles, missing generated
codec source coverage, and JSON/text legacy-gate discrepancies are failures.

Without optional coverage gates, low coverage is a measured result. Add
--enforce-legacy-line-gate to reject non-exempt codec line misses, or
--min-branch PERCENT for a measured incremental branch threshold. Historical
--compare-baseline fails on missing/failed units, changed inputs or changed
counts; it requires the complete historical configuration. A differing compiler
toolchain may legitimately change counts, but it must not silently pass an
exact-reproduction check. The checked-in reference is derived from the July
Phase-0 artifact, reverified on 2026-09-06; it contains no reachability claims.

The Ada build follows the generated coverage recipe but executes build, program
and gcov as separate checked stages. Formal SPARK proof is outside this lane,
even for inputs marked RUN_SPARK. Missing GNAT artifacts cannot silently skip
coverage. System headers outside the generated working tree are retained in raw
gcov but excluded from the summarized generated/runtime/harness scope.

## Existing project images

The root Dockerfile and Dockerfile.local.wsl provide the general regression
environment, including GNAT Community 2021 and Scala/SPARK tooling.
Dockerfile.runtime builds the compiler from upstream for its runtime image.
This lane instead builds the supplied checkout, uses GCC/GNAT/gcov 13 together
to reproduce Phase 0, and always runs as UID 10001. It does not change those
existing workflows. The existing asn1scc:latest image was also used for early
standard-library collector tests with an explicit non-root user.

The optional Makefile is run inside the container, for example:

    docker run --name coverage-make-pilot --network none --entrypoint make asn1scc-coverage:phase0b -f /opt/coverage/Makefile branch-coverage

## Collector checks (inside the same image)

    docker run --rm --network none --entrypoint python3 asn1scc-coverage:phase0b /opt/coverage/testCollector.py

Tests exercise failure reporting, timeout handling, missing profiles, partial
lines, exemption accounting, exact-source branch IDs and reference mismatch
detection. They do not require the host's Python or compiler.

Reference for gcov JSON semantics:
https://gcc.gnu.org/onlinedocs/gcc/Invoking-Gcov.html
