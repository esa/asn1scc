# Generated-code coverage

This lane builds asn1scc and measures its generated C/Ada code inside a Linux
container. Docker is the only required host tool. The final image runs as UID
10001 / GID 10001; source compilation also runs as that non-root user. No host
.NET SDK, Python, GCC, GNAT, gcov, or source-tree write mount is required.

## Build and run

From the repository root:

    docker build -f v4Tests/coverage/Dockerfile -t asn1scc-coverage:local .

The base SDK image is pinned by digest. Package revisions and the exact source
tree are recorded during the build; compiler bytes and tool versions are
recorded at runtime. Preserve the built image (and its Docker image ID) when
comparing runs. Rebuilding later may obtain newer Ubuntu package revisions.
An optional build argument SOURCE_REVISION records a known commit SHA; the
source-content hash is always recorded, including uncommitted supplied changes.
The Dockerfile-specific ignore file excludes host build output and local q/tmp
notes from the context. The precompiled ANTLR dependency DLLs remain inputs.

Run without network access. A named container retains reports even on failure:

    docker run --name coverage-inventory --network none asn1scc-coverage:local --outdir /results --inventory-only
    docker cp coverage-inventory:/results ./coverage-inventory

Do not add --rm if you intend to copy results afterwards. Use a new container
name per run. Every invocation creates a new report subdirectory and never
overwrites another run. After copying results you can remove that specific
finished container with docker rm.

Reproduce all original 259 units and compare individual counts and input hashes
(opt-in, using a compatible historical compiler and toolchain):

    docker run --name coverage-historical --network none asn1scc-coverage:local --outdir /results --cohort historical --compare-baseline
    docker cp coverage-historical:/results ./coverage-historical

Measure all runnable behavior-0 directives, including NOCOVERAGE:

    docker run --name coverage-all --network none asn1scc-coverage:local --outdir /results --enforce-legacy-line-gate
    docker cp coverage-all:/results ./coverage-all

Add --compare-baseline to an all-cohort run to compare the original 259 IDs
within the expanded measurement, without rerunning that cohort separately.
This is an exact reproducibility check, not the normal regression gate: newer
compiler revisions can legitimately change generated source and coverage counts.
The manual CI workflow always enforces the legacy line gate in each mode; its
`compare_historical` input enables the additional historical check explicitly.

The bounded pilot selects 12 directives covering integer, enum, strings/arrays,
CHOICE, shared determinants, OPTIONAL, parameter passing, CONTAINING and deduced
size. Run each mode separately so differences are visible:

    docker run --name coverage-c-pilot --network none asn1scc-coverage:local --outdir /results --cohort pilot
    docker run --name coverage-c-v2 --network none asn1scc-coverage:local --outdir /results --cohort pilot --acn-v2
    docker run --name coverage-ada-pilot --network none asn1scc-coverage:local --outdir /results --cohort pilot --language Ada
    docker run --name coverage-ada-v2 --network none asn1scc-coverage:local --outdir /results --cohort pilot --language Ada --acn-v2

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
Changing only the test harness preserves codec branch IDs, provided the codec
source and instrumentation toolchain remain the same. This supports attributing
coverage changes to individual harness operations.

Statement coverage is explicitly NOT measured by this gcov lane. The separate
statement images described below use GNATcoverage source instrumentation at
`--level=stmt`, with independent reports and tool provenance. The common image
does not provide gnatcov. Its line/block/branch data remains a complementary
diagnostic baseline; it is not statement or MC/DC evidence.
See the [GNATcoverage instrumentation guide](https://docs.adacore.com/gnatcoverage-docs/html/src_traces.html).

## Statement images and Docker orchestration

`Dockerfile.statement` has two runtime targets sharing the existing compiler
image. The initial statement lane supports Linux x86-64, native word size 8.

| Target / image tag | Instrumenter |
|---|---|
| `statement-ada` | Checksum-pinned GNATcoverage FSF 26.2 binary (Ada support) |
| `statement-c` | GNATcoverage 26.2 built with C support, using public AdaCore Clang bindings and LLVM 16 |

The C build uses the release-matched GNAT 15.2 compiler to build the tool.
Generated C/Ada programs still use the common image's GCC/GNAT 13 toolchain.
Both runtime images build the coverage runtime with that measurement toolchain
and must pass real source-trace calibration during their image build. Calibration
checks an uncovered statement sharing a line with covered statements, then
consolidates a second execution to cover it; corrupted XML reports are rejected.

Build the common image, then the two statement images. Save the expensive C
toolchain stage as a separate image so later compiler/runner changes can reuse it:

    docker build -f v4Tests/coverage/Dockerfile -t asn1scc-coverage:local .
    docker build -f v4Tests/coverage/Dockerfile.statement --target gnatcov-c-build -t asn1scc-gnatcov:26.2-c .
    docker build -f v4Tests/coverage/Dockerfile.statement --target statement-c --build-arg GNATCOV_C_IMAGE=asn1scc-gnatcov:26.2-c -t asn1scc-coverage:statement-c .
    docker build -f v4Tests/coverage/Dockerfile.statement --target statement-ada -t asn1scc-coverage:statement-ada .

`COVERAGE_IMAGE` can select another common image in each statement build.
Without `GNATCOV_C_IMAGE`, the C runtime target builds its instrumenter from
source. The first source build can take tens of minutes. Rebuild the toolchain
image when `gnatcov/build.py` or `gnatcov/sources.json` changes. The source pins,
archive hashes, build recipes and installed-package inventory are retained in
the C instrumenter's `toolchain-build.json`; preserve the final image ID too.
The common build stage includes Java to regenerate gitignored ANTLR parsers
from a clean checkout. No host compiler installation is required.

The host orchestrator requires only Bash and Docker. It selects an image by
metric and language, runs as UID/GID 10001 with no network or host mounts, and
exports results even on failure. It neither pulls images nor deletes containers:

    v4Tests/coverage/runCoverage.sh --metric stmt --language c --outdir coverage-results/stmt-c -- --cohort pilot
    v4Tests/coverage/runCoverage.sh --metric stmt --language Ada --outdir coverage-results/stmt-ada -- --filter 01-INTEGER/001.asn1#1
    v4Tests/coverage/runCoverage.sh --metric gcov --language c --outdir coverage-results/gcov-c -- --enforce-legacy-line-gate

`--image` overrides the selected image; `--name` gives the retained container
an explicit name. The default cohort is the bounded pilot. Supply collector
options after `--`; set language and output directory before it. Every host
output directory must be new. `container.json` records the actual image ID,
user, network mode, mounts and final exit status.

Run the original three-fixture checked/unchecked C comparison with statements:

    v4Tests/coverage/runCoverage.sh --metric stmt --language c --encode-pilot --outdir coverage-results/stmt-encode-pilot

This produces nine baseline/pilot pairs (uPER, legacy ACN and ACN-v2). It checks
unchanged codec sources and obligations, preserved positive tests and no lost
covered statements. The collector also verifies execution of every generated
unchecked encode call site. Zero statement gain is a valid measured result;
the earlier gcov branch gain is a different metric.

### Statement report scope

`statementCollector.py` retains commands, input/compiler hashes, source traces,
SID files, XML/xcov reports, individual statement obligations and summaries.
It cross-checks individual XML statements against GNATcoverage's per-file and
global **statement** statistics, rather than using line percentages. Missing
traces/SIDs, unmeasured codec bodies, zero executed tests, unknown/exempted
coverage statuses and report inconsistencies fail the run. Uncovered statements
are reported without an automatic percentage gate. Gcov gates and historical
gcov reference export/comparison are rejected by this separate lane.

Codec totals include generated codec/validation/equality/initialization sources.
The C encode/decode harness is instrumented for activation diagnostics and kept
separate from codec totals, as are any reported RTL headers. Counts accumulate
generated instances across configurations. IDs use unit/configuration, source
hash, statement spans and an occurrence ordinal; raw tool-local SCO IDs are
retained separately. They are not semantic goal IDs across source rewrites.
Nodes GNATcoverage marks as having no obligation remain explicitly listed.
`COVERAGE_IGNORE` and `NOCOVERAGE` do not exclude statement obligations.

Ada instrumented builds use GNATcoverage's documented configuration that ignores
SPARK proof-only pragmas, because inserted counters are not SPARK constructs.
The generated sources and runtime Pre/Post assertion policy are preserved.
This lane does not claim SPARK proof, assertion coverage or ghost-code coverage;
those are outside GNATcoverage's default `stmt` scope. See the
[SPARK instrumentation instructions](https://docs.adacore.com/gnatcoverage-docs/html/gnatcov/src_traces.html#instrumentation-and-coverage-of-spark-code).

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
Contract tests compare its flags with `StgAda/aux_a.stg` and compare the default
compiler flags with `runTests.py`. `NOCOVERAGE` exempts a unit from the legacy
line gate; both the collector and the regression runner still execute it.

## Exporting an exact reference

Export a new reference from a completed report directory without rerunning the
compiler (Python standard library only):

    python3 v4Tests/scripts/coverageCollector.py --from-run coverage-all/<run-id> --write-reference new-reference.json

The source run must be complete and successful: C, uPER + legacy ACN, word-size
8, non-slim, all or historical cohort, no filter or limit, and zero non-exempt
legacy line misses. The exporter checks inventory, unit statuses, input hashes
and summary totals; it retains compiler/toolchain provenance and artifact hashes.
An existing destination is rejected. Review a new reference before adopting it;
the checked-in historical reference remains unchanged.

Use `--reference new-reference.json --cohort historical --compare-baseline` to
reproduce the exported cohort. Here `historical` means the exact unit IDs in the
supplied reference, which may differ from the original 259-unit cohort.

## Existing project images

The root Dockerfile and Dockerfile.local.wsl provide the general regression
environment, including GNAT Community 2021 and Scala/SPARK tooling.
Dockerfile.runtime builds the compiler from upstream for its runtime image.
This lane instead builds the supplied checkout, uses GCC/GNAT/gcov 13 together
to reproduce Phase 0, and always runs as UID 10001. It does not change those
existing workflows. The existing asn1scc:latest image was also used for early
standard-library collector tests with an explicit non-root user.

The optional Makefile is run inside the container, for example:

    docker run --name coverage-make-pilot --network none --entrypoint make asn1scc-coverage:local -f /opt/coverage/Makefile branch-coverage

## Collector checks (inside the same image)

    docker run --rm --network none --entrypoint python3 asn1scc-coverage:local /opt/coverage/testCollector.py
    docker run --rm --network none --entrypoint python3 asn1scc-coverage:local /opt/coverage/testContracts.py

Tests exercise failure reporting, timeout handling, missing profiles, partial
lines, exemption accounting, exact-source branch IDs and reference mismatch
detection. They do not require the host's Python or compiler.

## Ada/v2 CONTAINING regressions

The additional integration check compiles `24-DEDUCED-SIZE/007.asn1` and checks
exact wire bytes, padding before following fields, adjacent and empty regions,
acceptance of nonzero padding, and rejection of malformed length determinants.
It uses the public decoders with full-capacity streams. It is a runtime check;
it does not establish SPARK proof or an assertion-enabled decoding contract.

    docker run --rm --network none --entrypoint python3 asn1scc-coverage:local /opt/coverage/testAdaContaining.py
    docker run --rm --network none --entrypoint python3 asn1scc-coverage:local /opt/coverage/testAdaContaining.py --slim

For a host with the compiler and GNAT installed, run
`python3 v4Tests/coverage/testAdaContaining.py`. Optional `--compiler` and
`--test-root` select another build/corpus. `--outdir` preserves generated code,
commands and diagnostics in a new directory.

## NULL codec regressions

This check removes the value assignment from `18-NULL/001.asn1#1` and requires
two real automatic tests (uPER and ACN). A separate Ada driver verifies that
public and auxiliary NULL decoders define their outputs, encode/decode consume
zero bits, and auxiliary calls preserve an existing non-byte-aligned stream.
It runs legacy/v2, each in normal/slim mode, with the ordinary strict build flags.
Each mode also tests a NULL alias selected with `-icdPdus`, ensuring that
transitively needed codec initializers are emitted (four automatic tests).
The selected-alias driver also runs without `-atc`, checking the production
codec dependencies independently of the test harness.

    docker run --rm --network none --entrypoint python3 asn1scc-coverage:local /opt/coverage/testNullCodecs.py

Measure the original fixture with statements after rebuilding the common and
Ada statement images. The all cohort is needed because the pilot excludes it:

    v4Tests/coverage/runCoverage.sh --metric stmt --language Ada --outdir coverage-results/null-stmt -- --cohort all --filter '18-NULL/001.asn1#1' --acn-v2
    v4Tests/coverage/runCoverage.sh --metric gcov --language Ada --outdir coverage-results/null-gcov -- --cohort all --filter '18-NULL/001.asn1#1' --acn-v2 --enforce-legacy-line-gate

## Initializers below references

The `16-mantis/0231` regression requires all 12 generated round trips and runtime
coverage of the four element initializers below constrained array references.
It checks the existing line gate in Ada legacy/v2, normal/slim. A separate driver
checks initializer values, parent/element agreement and constraints; its calls
do not contribute to the automatic suite's coverage evidence.

    docker run --rm --network none --entrypoint python3 asn1scc-coverage:local /opt/coverage/testReferenceInitializers.py

Measure statements independently with the statement image and the all cohort:

    v4Tests/coverage/runCoverage.sh --metric stmt --language Ada --outdir coverage-results/reference-stmt -- --cohort all --filter '16-mantis/0231.asn1#1' --acn-v2

## Bounded C encode pilot

`--check-encode` enables `ASN1SCC_CHECK_ENCODE` when compiling the generated C
harness. After each successful checked binary encode, the harness encodes the
same valid value with constraint checking disabled, into a separate buffer of
the same capacity. It requires a true return value, identical byte/bit
cursors and identical encoded bits. Unused low bits of the final byte are not
compared. The shared-presence ACN fixture can leave a nonzero error code on a
successful encode; the comparison uses the Boolean return value for success.
The original checked stream is retained for the ordinary round trip.
Error stage 5 identifies an unchecked-encode failure or mismatch. XER keeps its
ordinary harness. The option is off by default and is rejected for Ada and
historical comparison; pilot runs cannot be exported as standard references.

Run the bounded comparison (three directives, each in uPER, legacy ACN and
ACN-v2, with checking enabled only versus both modes):

    python3 v4Tests/coverage/encodePilot.py --outdir /tmp/encode-pilot

Or inside a newly built coverage image:

    docker run --name encode-pilot --network none --entrypoint python3 asn1scc-coverage:local /opt/coverage/encodePilot.py --outdir /results/pilot

The directives are `01-INTEGER/001.asn1#1`, `06-OCTET-STRING/004.asn1#2` and
`10-SEQEUENCE/008.asn1#1`: scalar constraints, variable-length octets and shared
OPTIONAL presence. `pilot.json` records each pair's codec counts, individual
newly covered branch arms, test/additional-call counts and execution wall time.
Each run retains the collector's source/tool hashes, commands and raw evidence.
The comparison requires unchanged codec sources/branch identities and positive
test counts, one additional encode per test, no lost branch arms or line-gate
regression, and a positive branch gain for each selected unit/configuration.
These are bounded pilot checks, not a general coverage percentage target.
Short process wall times include startup and collector polling overhead; they
are not a precise per-encode benchmark. No statement coverage is measured.

Check the comparison oracle with fault injection in scratch copies of the
generated integer/ACN pilot's work directory:

    python3 v4Tests/coverage/testEncodeHarness.py --work /path/to/integer-acn/units/UNIT/work --outdir /tmp/encode-oracle

This checks encode failure, error status, byte/bit lengths, full/partial-byte
content and permitted padding differences under ASan/UBSan. It does not modify
the retained coverage run. Truncation, invalid values and unequal-value tests
remain later harness operations.

Reference for gcov JSON semantics:
https://gcc.gnu.org/onlinedocs/gcc/Invoking-Gcov.html
