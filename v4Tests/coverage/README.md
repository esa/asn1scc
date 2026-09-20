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

### Bounded C decode pilot

The decode pilot measures three stages independently: the existing positive
round trips, the same tests with actual encoded byte-length attachment, then
those tests plus four explicit prefix checks per configuration. Enable only
actual-length attachment with `--decode-stage actual`; `truncate` additionally
requires an explicitly supported unit/encoding. The generated test template
uses `ASN1SCC_DECODE_ACTUAL_LENGTH`; codec and RTL bodies are unchanged.
Lengths come from `BitStream_GetLength` and are rounded-up **bytes**, not bits.

    v4Tests/coverage/runCoverage.sh --metric gcov --language c --image asn1scc-coverage:decode-pilot-base --decode-pilot --outdir coverage-results/decode-gcov
    v4Tests/coverage/runCoverage.sh --metric stmt --language c --image asn1scc-coverage:decode-pilot-statement-c --decode-pilot --outdir coverage-results/decode-stmt

Build the common image with the first tag above, then build `statement-c` using
`COVERAGE_IMAGE=asn1scc-coverage:decode-pilot-base` and the reusable
`GNATCOV_C_IMAGE=asn1scc-gnatcov:26.2-c`, as described above. Rebuild both images
after generator changes; always use new output directories.

The five configurations are `10-SEQEUENCE/008.asn1#1` (uPER), and
`24-DEDUCED-SIZE/002.asn1#1` plus `24-DEDUCED-SIZE/001.asn1#2` (legacy ACN and
ACN-v2). The first targets the enum decode-failure fallback; the second targets
the deduced list's residual-byte error statements. The fixed-element list is
a control for valid shorter prefixes. `decodeHarness.py` records each prefix,
expected error, value and consumed length; this whitelist is not a generic
"every truncated message must fail" rule. No automatic tests are deduplicated.

`pilot.json` preserves separate baseline→actual and actual→truncate comparisons,
positive-test counts, prefix outcomes, execution times and newly covered
obligations/branch arms. Codec source and instrumentation identities must remain
unchanged, with no lost coverage. Statement checks require the selected target
statements to become covered. A zero gain at the actual-length stage is valid.
Each executable is limited to 30 seconds; each configuration adds one encode
and four decodes in the truncation stage, independently of encoded size.

Run ASan/UBSan and failure-injection checks against the retained pilot artifacts:

    docker run --name decode-sanitizers --network none -v "$(pwd)/coverage-results/decode-gcov/results/pilot:/evidence:ro" --entrypoint python3 asn1scc-coverage:decode-pilot-base /opt/coverage/testDecodePilot.py --pilot-root /evidence --outdir /results/checks
    docker cp decode-sanitizers:/results/checks coverage-results/decode-sanitizers

These rebuild uninstrumented generated sources with sanitizers, use exact-size
prefix allocations (a poisoned sentinel for length zero), and require the
oracles to reject wrong acceptance, rejection, error, value and consumption.
A deliberate read beyond the view must be detected by ASan. Missing prefix
executions must also fail collection. Gcov accounts for the driver as harness.
The statement lane keeps the same codec/auto-test-helper instrumentation scope
across stages and checks driver execution through the per-prefix results.

### Bounded C invalid-value pilot

`--invalid-value-pilot` compares existing positive tests with the same tests plus
explicit invalid-value checks. It uses `09-CHOICE/003.asn1#3` and
`09-CHOICE/011.asn1#1`, each with uPER + legacy ACN and uPER + ACN-v2. Each of
the four configurations adds two invalid values and six API checks: direct
validation, checked uPER encode and checked ACN encode for each value.

    v4Tests/coverage/runCoverage.sh --metric gcov --language c --image asn1scc-coverage:invalid-values-base --invalid-value-pilot --outdir coverage-results/invalid-gcov
    v4Tests/coverage/runCoverage.sh --metric stmt --language c --image asn1scc-coverage:invalid-values-statement-c --invalid-value-pilot --outdir coverage-results/invalid-stmt

Build these tags using the common and C statement image recipes above. Compiler
templates, generated codecs and RTL are unchanged by this pilot; the collector
adds a fixture-specific driver to the generated measurement harness. The default
positive tests remain unchanged. A standalone unit can opt in with
`--check-invalid-values` (C/both only); unknown fixtures and combinations with
other harness operations are rejected.

The statement targets are the validator's two default-rejection statements for
each CHOICE. The unset kind is an explicitly declared C `*_NONE` enumerator,
not an invented out-of-range enum representation. A nested integer value 16
outside 0..15 and a subtype's forbidden `field1` alternative are separate
controls; their rejection may add branches without adding statements.
All encoder calls use constraint checking. Tests require the exact expected
error, false return, unchanged cursor/count and unchanged output buffer.
Invalid objects are never passed to Equal or an unchecked encoder.

Reports preserve the original positive counts, per-value/API outcomes, run
times, unchanged codec hashes and obligation identities, and exact newly covered
validator targets. Statements and gcov metrics remain separate. Every pilot
stage has a 30-second timeout. The `NOCOVERAGE` marker on the restricted fixture
retains its existing line-gate meaning and does not exempt statements.

Run sanitizer and deliberate-failure checks on the exported artifacts:

    docker run --name invalid-value-sanitizers --network none -v "$(pwd)/coverage-results/invalid-gcov/results/pilot:/evidence:ro" --entrypoint python3 asn1scc-coverage:invalid-values-base /opt/coverage/testInvalidValuePilot.py --pilot-root /evidence --outdir /results/checks
    docker cp invalid-value-sanitizers:/results/checks coverage-results/invalid-sanitizers

The checks exercise wrong acceptance/errors, buffer writes, cursor movement,
missing value execution, and removal of each selected validator statement.
This is a bounded Phase-2b pilot, not general invalid-value synthesis or a
claim that every defensive encoder branch is reachable by checked calls.

#### Extra CHOICE goals over existing stream checks

`invalidValueHarness.EXTRA_GOALS` enables `choice-unset` for
`09-CHOICE/001.asn1#1` and `payload-unset` for `09-CHOICE/013.asn1#1`.
The public value pilot appends these units in legacy ACN and ACN-v2 after its
four historical uPER+ACN configurations (indices 0–3); choice-unset retains
indices 4–5 and payload-unset uses 6–7. Both stages include all existing stream
checks; only the candidate adds the value goals. These comparisons measure value coverage over
stream coverage. The historical positive-only deltas are reported separately
and must not be claimed as new campaign gains.

`prepare_extra(work, unit, args)` wraps the prepared driver's `main`, runs it
once, then executes extra operations only after it succeeds. For each operation,
the driver initializes and validates a fresh parent, changes only its CHOICE
kind and resets the error. For `choice-unset`, it sets `MyPDU_NONE` and requires
exactly `ERR_MYPDU`. For `payload-unset`, it changes only `payload.kind` to
`MyPayload_NONE`; direct MyPayload validation, parent MyPDU validation and the
parent's checked ACN encoding must each reject with exactly `ERR_MYPAYLOAD`.
The parent's initializer assigns its constant directly, leaving the separate
MyPayload initializer goal disabled. The checked
encoder must preserve every output byte, both cursor fields and the buffer count.
Direct and parent validation coincide for this top-level CHOICE but run
independently. No invalid value reaches Equal or an unchecked encoder.

Each operation emits exactly `Value goal GOAL/OPERATION: OK`, where GOAL is the
enabled goal for the selected unit and
OPERATION is `validate`, `validate-parent` or `encode`. `verify_extra_output`
rejects missing, duplicate and unexpected goal lines. Metadata retains ordered
goal IDs, operations, source-mapped validator targets and the driver hash.
Unregistered units receive no extra checks or transcript lines.

The pilot's scoped loader adapter preserves/restores both the collector loader
and exact cohort selector, including on failure. Collector CLI restrictions
remain active: `--check-invalid-values` still requires C/both and cannot combine
with `--check-invalid-streams`. Use the public pilot to compose the checks:

    python3 /opt/coverage/invalidValuePilot.py --metric gcov --outdir /results/value-pilot
    python3 /opt/coverage/invalidValuePilot.py --metric stmt --outdir /results/value-statements
    python3 /opt/coverage/testInvalidValuePilot.py --pilot-root /results/value-pilot --outdir /results/value-checks

Run these commands inside the appropriate coverage images containing the current
scripts, or mount the three updated value Python files read-only over their
individual `/opt/coverage` paths, preserving the image's collector dependencies. Optional
`--unit '09-CHOICE/013.asn1#1'` selects exactly the two payload configurations
(`09-CHOICE/001.asn1#1` selects the choice pair);
the sanitizer suite consumes a complete gcov pilot. `pilot.json` includes
`extra_goals`, per-run operation metadata and relative work-directory paths.
The strict legacy line gate stays active for every gcov stage.

The sanitizer suite retains all historical checks and adds per-goal/per-mode
ASan/UBSan runs, actual omission of each operation, wrong results and exact-error
faults for each call, byte writes, changes to each cursor/count field, and removal
of each target validator assignment. Faults modify disposable copies only;
coverage measurements retain identical generated codec, RTL and ATC sources.
`checks.json` records goal, mode, fault name, operation and status.

### Bounded C invalid-stream pilot

`--invalid-stream-pilot` measures baseline and fixed-layout mutations for
every unit registered in `invalidStreamHarness.SUPPORTED_UNITS`, separately in
legacy ACN and ACN-v2. The six enum layouts retain their alpha/beta wire mappings:

| Unit | Wire field at bit 0 | Alpha / beta codes | Invalid enum codes | Padding bits |
|---|---|---|---|---|
| `04-ENUMERATED/001.asn1#1` | 10-bit positive integer | 50 / 60 | 51, 0, 1023 | 6 |
| `04-ENUMERATED/001.asn1#2` | 12-bit BCD (three decimal nibbles) | 50 / 60 | 51, 0, 999 | 4 |
| `04-ENUMERATED/001.asn1#3` | 32-bit ASCII (four decimal bytes) | 50 / 60 | 51, 0, 9999 | 0 |
| `04-ENUMERATED/001.asn1#5` | 10-bit positive integer | 1 / 200 | 51, 0, 1023 | 6 |
| `04-ENUMERATED/002.asn1#1` | 10-bit two's complement | -1 / -200 | -2, 0, -512, 511 | 6 |
| `04-ENUMERATED/002.asn1#2` | 32-bit signed ASCII (sign + three digits) | -1 / -200 | -2, 0, -999, 999 | 0 |

Both valid values are encoded with constraint checking before mutating their
bytes. BCD 050/060 encode alpha/beta; 051 is a valid BCD number rejected by the
generated enum switch. No malformed decimal digit is needed to reach the target.
The driver checks canonical seed bytes with zero padding: #1 uses `0C 80` /
`0F 00`, #2 uses `05 00` / `06 00`, and #5 uses `00 40` / `32 00`.
Unit #5 reuses the same field operations as #1 with its native numeric codes.
ASCII #3 uses exactly `30 30 35 30` / `30 30 36 30` (`0050` / `0060`),
without a terminator or padding. Its mutations are four valid decimal bytes
(`0051`, `0000`, `9999`, or the other valid code), checked digit by digit and
as a decimal value before decoding.
Signed `002#1` uses `FF C0` / `CE 00`. Its four invalid field values use
`FF 80`, `00 00`, `80 00`, and `7F C0` with zero padding. The harness keeps
signed values separate from unsigned wire bits: it subtracts 1024 from a
representable positive integer when reading the sign bit, and adds 1024 to
negative values before converting to unsigned bits for writing. No negative
integer is shifted and no out-of-range unsigned value is cast to signed.
Signed ASCII `002#2` uses exactly `2D 30 30 31` / `2D 32 30 30`
(`-001` / `-200`), with no terminator or padding. Its invalid wires are
`-002`, `+000`, `-999`, `+999`. The reader checks the sign and three digits;
the writer uses a representable magnitude bounded by 999. Before each decode,
the driver checks both the signed numeric value and all four canonical bytes,
including the positive sign on zero.

Use images containing the current tooling with the common/C statement recipes
above and fresh development tags; do not overwrite existing measurement images.
For example, after building those tags:

    v4Tests/coverage/runCoverage.sh --metric gcov --language c --image asn1scc-coverage:enum-streams-dev-base --invalid-stream-pilot --outdir coverage-results/streams-gcov
    v4Tests/coverage/runCoverage.sh --metric stmt --language c --image asn1scc-coverage:enum-streams-dev-statement-c --invalid-stream-pilot --outdir coverage-results/streams-stmt

For tooling-only development, mount the checkout's coverage scripts read-only
under `/variant` in an existing compiler image:

    docker run --name streams-dev-gcov --network none --user 10001:10001 --mount type=bind,source="$(pwd)/v4Tests",target=/variant,readonly --entrypoint python3 asn1scc-coverage:invalid-streams-base /variant/coverage/invalidStreamPilot.py --metric gcov --outdir /results/pilot
    docker cp streams-dev-gcov:/results/pilot coverage-results/streams-dev-gcov

For statements, use `asn1scc-coverage:invalid-streams-statement-c`, `--metric stmt`
and a fresh container name/output destination. This pilot changes
only measurement tooling and adds an optional driver to the generated test main;
compiler templates, codecs, RTL and original positive tests are unchanged.
`--check-invalid-streams` selects the driver for an individual C/ACN measurement.
For standalone #1 use `--cohort pilot --filter 04-ENUMERATED/001.asn1#1` so the
collector's substring filter does not also select #10/#11. For #2, #3 or #5 use
`--cohort all --filter 04-ENUMERATED/001.asn1#2` (or `#3` / `#5`). The pilot itself supplies an
exact one-unit cohort and validates the resulting unit identity. Unknown fixtures,
other encodings/languages and combinations with other harness operations are rejected.

Each unsigned seed gets five decoder checks: original, the three invalid codes above (all
rejected with FALSE and exactly `ERR_ACN_DECODE_MYPDU`), and the other valid code.
Units `001#1/#2/#5` also get a sixth, padding-only check. Signed `002#1` gets
seven checks: original, four invalid codes, other-valid and padding-only.
Signed ASCII `002#2` gets six: original, four invalid codes and other-valid.
Their field mutations preserve
all unused padding bits; the separate padding control flips one unused bit and
must still decode to the original value. ASCII has no padding check. Every view
is exactly two bytes for the binary/BCD layouts or four bytes for ASCII. The oracle verifies the
encoded field/layout, mutated field and any padding, exact result/error/value,
10, 12 or 32 consumed bits, unchanged view length and unchanged decoder input.
Stream/output/error state is reset for every call; positive outputs start at
the other valid enum value.

All original automatic-test suites remain intact. The pilot adds two checked
encodes per configuration. Units #1/#2/#5 have twelve decodes (six negative,
six positive controls); ASCII #3 has ten (six negative, four positive controls).
Signed `002#1` has fourteen decodes (eight negative, six positive controls).
Signed ASCII `002#2` has twelve (eight negative, four positive controls).
All stages have a 30-second timeout. Reports retain source hashes,
obligation/branch identities, no-loss comparisons and the exact two decoder
default-rejection statements that must become covered. Existing NOCOVERAGE
status affects only the legacy line gate; statements remain unexempted.
`pilot.json` identifies every comparison/run by unit and mode, records the exact
baseline/mutation work directories for each comparison, and records each run's
relative directory for the sanitizer runner. Pilot baselines use only
original positives: gains for already-supported units are historical and must
not be counted as new progress against a base that includes their mutations.
Campaign comparisons must retain all mutations supported by the committed base.
The signed campaign uses six units (`001#1/#2/#3/#5`, `002#1/#2`) in both modes,
all now registered. Its initial totals were 400/432 statements and 208/336 branch
arms. The accepted signed-binary task reached 404/432 and 212/336; that committed
base retains all five existing units' negatives for the signed ASCII comparison.
The ASCII task targets four additional decoder statements and a positive branch
gain, preserving 36 original positive ATCs per snapshot. Historical positive-only
pilot gains are not new campaign gains. This six-unit scope does not establish
whole-project coverage completion.

    docker run --name stream-sanitizers --network none --user 10001:10001 --mount type=bind,source="$(pwd)/v4Tests",target=/variant,readonly -v "$(pwd)/coverage-results/streams-dev-gcov:/evidence:ro" --entrypoint python3 asn1scc-coverage:invalid-streams-base /variant/coverage/testInvalidStreamPilot.py --pilot-root /evidence --outdir /results/checks
    docker cp stream-sanitizers:/results/checks coverage-results/streams-sanitizers

ASan/UBSan runs use the layout's exact two- or four-byte buffers for the twelve
enum unit/mode configurations.
Deliberate defects check acceptance, errors, decoded values, writes, consumption,
view counts, field offsets, padding, truncated views, skipped cases and removal
of either target statement. Padding faults apply to every layout with unused
bits; signed binary also checks unsigned interpretation and loss of the sign bit.
Signed ASCII checks ignored signs, a negative-to-positive sign mutation, and
noncanonical negative zero (which a numeric-only comparison would miss).
The other faults apply to ASCII as well. Missing-case injection omits the final expected
case for each layout and must be rejected by transcript verification.
This is a bounded
Phase-3 pilot, not generic mutation synthesis or a claim about all decoder errors.

#### Compound present-when CHOICE

The same registry also supports `09-CHOICE/013.asn1#1`, in both modes.
`STREAM_PROFILES` describes typed seed assignments, canonical bytes, field
offsets/widths, bounded mutations, positive field checks and rejection targets.
The shared driver applies bit-field operations to checked encodings and compares
the entire resulting wire against a separately constructed byte oracle before
decoding. Case behavior is named in descriptors, not selected by test position.
The original enum APIs, metadata and transcripts remain available.

This profile encodes two valid seeds: `alt-17-1` (`01 10 1A 55 A0`, 36 bits)
and `alt-20-1` with one parameter `0x1234`
(`01 40 1A 50 11 23 45 A0`, 60 bits). Both use zero flags, sourceId `0xA5`
and crc `0x5A`. Each seed has six cases: original; service/message selectors
`(18,1)`, `(20,2)`, `(0,0)`, `(255,255)`; and a low padding-bit flip.
Only the selector fields at bits 4 and 12 (eight bits each) change in rejection
cases. Four low padding bits are present in each seed's last byte.

Each configuration adds two checked encodes and twelve decodes: eight reject
after exactly 28 bits with the generated payload decoder error, and four succeed
after exactly 36 or 60 bits with error zero and the expected fields. Every call
uses a fresh exact-size five- or eight-byte buffer, stream, output and error.
Positive output starts with different fields; failed output is never validated
or passed to Equal. Input bytes and view counts must remain unchanged.
ACN-v2's private payload error definition is resolved uniquely from `sample1.c`
and supplied only to the optional main driver. Codec/header files are untouched.

New-profile metadata includes the exact unit, ordered case IDs, encode/decode
counts and source-mapped target lines. Each case prints exactly one
`Stream profile UNIT/CASE: expected success, OK` or `expected rejection, OK`
line. Transcript verification rejects missing, duplicate, reordered or unexpected
cases. Fault checks cover acceptance, error, positive fields, input writes,
consumption, view count, selector offset, padding corruption/missing flip,
short views, genuinely omitted execution and removal of either generated target
assignment. Fault names map to source statements independently of their order.
The full sanitizer suite covers every registered unit/mode configuration.

`invalidStreamPilot.py --unit '09-CHOICE/013.asn1#1'` selects exactly this unit;
omitting `--unit` measures all registered units. Positive-test counts come from
each generated suite. Target-line lists are profile-specific, with the old enum
mapping preserved. Reports retain the existing schema and work directories.

The public pilot still compares original positives with added mutations. Its
old enum gains are historical. Incremental campaign evidence must compare the
actual committed base (including all six enum profiles) with the candidate
across the fixed fifteen-unit, thirty-configuration cohort. Statement and gcov
branch results must remain separate; the CHOICE target is two rejection/error
statements per mode, with positive branch gains and no coverage loss anywhere.
This does not establish whole-project completeness or unreachable statements.

For a frozen campaign, use its pinned images and read-only tooling mounts,
fresh output paths, no network and the campaign container label. For example:

    docker run --name choice-streams-dev-UNIQUE --label asn1scc.campaign=coverage-night-20260917 --network none --user 10001:10001 --mount type=bind,source="$(pwd)/v4Tests",target=/variant,readonly --entrypoint python3 asn1scc-coverage:invalid-streams-base /variant/coverage/invalidStreamPilot.py --metric gcov --outdir /results/pilot

Use `invalid-streams-statement-c` with `--metric stmt` for statement measurements.
Preserve raw reports, generated sources, hashes and sanitizer/fault logs before
removing a finished container. The campaign's independent verifier additionally
checks the full committed-base comparison and canonical wire/source-fault oracles.

#### External eight-bit OCTET STRING length

`06-OCTET-STRING/004.asn1#2` extends the common registry to nine units, each in
legacy ACN and ACN-v2. Its checked seed has `a2.nCount=4` and octets
`AF BC 45 83`: encode must return TRUE/error zero and exactly
`04 AF BC 45 83` (40 bits). Each mode adds one encode and these five decodes:

| Case | Length byte | Result / error | Consumed bits | Successful value |
| --- | --- | --- | --- | --- |
| original | 4 | TRUE / 0 | 40 | Count 4, `AF BC 45 83` |
| length0 | 0 | FALSE / 0 | 8 | — |
| length21 | 21 | FALSE / `ERR_ACN_DECODE_MYPDU_A2` | 8 | — |
| length255 | 255 | FALSE / `ERR_ACN_DECODE_MYPDU_A2` | 8 | — |
| valid-shorter | 1 | TRUE / 0 | 16 | Count 1, `AF` |

Every case retains the actual five-byte input, with no padding and no extra
payload for oversized lengths. The lower-bound FALSE/error-zero result is an
exact fixture oracle. A grammar-valid shorter value succeeds without consuming
the entire view. Named case descriptors carry independent consumption, error
and value expectations; the shared driver resets output, stream and error and
checks unchanged input bytes and view count on every call.

This profile has `target_lines=[]`: the incremental goal is three new codec
branch arms per mode and zero new statements. Source-fault mapping is separate
from statement targets and checks removal of the A2 decoder error assignment.
The public fault suite also checks wrong lengths, full-view consumption imposed
on a shorter result, field offsets, short views and all common outcome/value/
input/accounting defects. No padding fault is registered for this unit.

Both public pilot metrics include it automatically; exact selection is
`--unit '06-OCTET-STRING/004.asn1#2'`. Preserve all eight previous profiles and
the PUS initializer control in both stages, including the strict line gate.
For the daytime campaign use `--label asn1scc.campaign=coverage-day-20260918`
with the unchanged pinned images, no network and read-only source mounts.
Measure incremental gains against the actual committed campaign base across
all 30 cohort configurations; historic positive-only pilot gains are separate.

#### External five-bit OCTET STRING length and padding

`06-OCTET-STRING/004.asn1#1` brings the registry to ten units in both legacy
ACN and ACN-v2. It uses the same checked logical seed as the eight-bit profile,
but its five-bit determinant precedes an unaligned payload. Encoding returns
TRUE/error zero with exactly `25 7D E2 2C 18` and 37 bits; the five-byte view
includes three trailing padding bits. One checked encode feeds six decodes:

| Case | Exact input bytes | Result / error | Consumed bits | Successful value |
| --- | --- | --- | --- | --- |
| original | `25 7D E2 2C 18` | TRUE / 0 | 37 | Count 4, `AF BC 45 83` |
| length0 | `05 7D E2 2C 18` | FALSE / 0 | 5 | — |
| length21 | `AD 7D E2 2C 18` | FALSE / `ERR_ACN_DECODE_MYPDU_A2` | 5 | — |
| length31 | `FD 7D E2 2C 18` | FALSE / `ERR_ACN_DECODE_MYPDU_A2` | 5 | — |
| valid-shorter | `0D 7D E2 2C 18` | TRUE / 0 | 13 | Count 1, `AF` |
| padding | `25 7D E2 2C 19` | TRUE / 0 | 37 | Count 4, `AF BC 45 83` |

Shared bounded field mutations replace only bits 0–4; the padding operation
flips only the final bit. Every case retains exactly five input bytes, with
no extra payload for oversized determinants. The lower-bound FALSE/error-zero
contract and shorter value's 13-bit consumption are checked explicitly.
Metadata records the ordered case IDs above, one encode, six decodes, three
rejections, three successes and `target_lines=[]`.

The public sanitizer/fault suite automatically includes this profile's real
padding controls (`changed-padding`, `missing-padding-flip`), length, value,
consumption, view, field-offset, encoding-error and omitted-case checks. Its
shared source-fault mapping detects removal of the A2 error assignment.
Both public pilot metrics run all twenty registered configurations; exact
selection is `--unit '06-OCTET-STRING/004.asn1#1'`. All nine preceding profiles
and the PUS initializer controls remain active. The incremental target is
three previously uncovered codec branch arms per mode, zero new statements
and no coverage losses against the actual committed base; the earlier
eight-bit profile's gains are already part of that base.

#### Three-bit CHOICE index

`09-CHOICE/001.asn1#1` brings the shared registry to eleven units in legacy
ACN and ACN-v2. Both checked seeds select `MyPDU_int1_PRESENT`, with integer
values 10 and 11, encoding as `14` and `16` respectively (hexadecimal, seven
bits, TRUE/error zero). Bits 0–2 contain the selector, bits 3–6 the integer,
and bit 7 is trailing padding. Every decode has an exact one-byte buffer/view.

The shared case-name format adds `seed0-` or `seed1-` to these operations,
in this order for each seed:

| Case suffix | Seed 0 input | Seed 1 input | Result / error | Consumed bits |
| --- | --- | --- | --- | --- |
| original | `14` | `16` | TRUE / 0 | 7 |
| index5 | `B4` | `B6` | FALSE / `ERR_ACN_DECODE_MYPDU` | 3 |
| index6 | `D4` | `D6` | FALSE / `ERR_ACN_DECODE_MYPDU` | 3 |
| index7 | `F4` | `F6` | FALSE / `ERR_ACN_DECODE_MYPDU` | 3 |
| padding | `15` | `17` | TRUE / 0 | 7 |

Bounded field replacement preserves the payload and padding; the padding
operation flips only bit 7. Successful decodes must preserve both the selected
alternative and the seed's integer value. Metadata records two checked encodes,
ten decodes, six rejections, four successes, and the ten ordered case IDs.
Each case resets output/error/stream and checks exact result, error, consumption,
input bytes and view count. Transcript verification rejects missing or duplicate
cases, including genuinely omitted execution in the public fault suite.

Invalid indexes fail the constrained read before entering the CHOICE switch.
Thus `target_lines=[]`; the incremental goal is three branch arms per mode
and zero new codec statements. Shared source-fault mapping detects removal of
`*pErrCode = ret ? 0 : ERR_ACN_DECODE_MYPDU;`. The existing private-error helper
resolves the generated definition for the optional driver. ASan/UBSan and all
applicable shared outcome, value, encoding-error, input, consumption, view,
field-offset, short-view and padding faults run for both modes.

Both public pilot metrics include all twenty-two registered configurations;
`--unit '09-CHOICE/001.asn1#1'` selects this unit exactly. All ten preceding
profiles, PUS initializer controls and strict line gates remain active.
Measure this branch-only increment against the actual committed campaign base
over all thirty cohort configurations; earlier pilot gains remain separate.

#### Three-bit BOOLEAN patterns

`05-BOOLEAN/003.asn1#1` extends the shared registry to twelve units in both
legacy ACN and ACN-v2. Checked encoding of TRUE and FALSE produces `20` and
`00` respectively (hexadecimal, three bits, TRUE/error zero). Every input
buffer and view is exactly one byte; every decode consumes three bits.

For each seed, the shared operations run in this order with `seed0-` or
`seed1-` prefixed to the case name:

| Case suffix | TRUE seed input | FALSE seed input | Result / error |
| --- | --- | --- | --- |
| original | `20` | `00` | TRUE / 0 |
| pattern2 | `40` | `40` | FALSE / `ERR_ACN_DECODE_MYPDU` |
| pattern7 | `E0` | `E0` | FALSE / `ERR_ACN_DECODE_MYPDU` |
| padding | `21` | `01` | TRUE / 0 |

Invalid cases replace only bits 0–2; padding controls flip only bit 7, within
the five actual trailing padding bits. Successful decodes must reproduce the
seed's logical value. Shared value descriptors use an empty field path for a
scalar and optional `initial` assignments on seeds/cases to reset decode output.
BOOLEAN starts from the opposite logical value for every case, including FALSE
positives. The existing check that positive output initially differs remains
active; structured profiles retain zero initialization.

Metadata records two checked encodes, eight ordered case IDs, four rejections,
four successes and `target_lines=[]`. Exact result/error/value/consumption/view
and unchanged-input checks apply to every call. Shared source-fault mapping
detects removal of `*pErrCode = ret ? 0 : ERR_ACN_DECODE_MYPDU;`. The sanitizer
suite retains all shared faults, including real case omission and padding
faults, and separately corrupts TRUE and FALSE positive values.

Both public metrics include all twenty-four registered configurations;
`--unit '05-BOOLEAN/003.asn1#1'` selects this unit exactly. The incremental
coverage contract is two additional branch arms per mode and zero new codec
statements, measured against the committed base across all thirty cohort
configurations. Prior profiles, original positives, PUS initializer controls,
strict line gates and source-identity checks remain required. Positive-only
public pilot deltas do not measure this increment.

#### Three-bit NULL pattern

`18-NULL/001.asn1#2` extends the shared registry to thirteen units in legacy
ACN and ACN-v2. Checked encoding of the valid NULL representation `value=0`
produces `20` (hexadecimal), returning TRUE/error zero and consuming three
bits. Every encode buffer, decode buffer and input view is exactly one byte.

The four cases execute in this exact order; every decode consumes three bits:

| Case | Input | Result / error |
| --- | --- | --- |
| original | `20` | TRUE / 0 |
| pattern0 | `00` | FALSE / `ERR_ACN_DECODE_MYPDU` |
| pattern1 | `E0` | FALSE / `ERR_ACN_DECODE_MYPDU` |
| padding | `21` | TRUE / 0 |

The named `pattern1` case replaces bits 0–2 with numeric value 7. Both invalid
patterns use the shared bounded field operation; padding flips only the least
significant bit among the five genuine trailing padding bits. Output, error
and stream are reset for every case. Exact result, error, consumption, input
bytes and view count are checked, with complete and unique case transcripts.

A profile value descriptor of `None` explicitly marks logical-value checks as
inapplicable. NULL has no positive-value assertion, initially-different value
or value-corruption fault. Non-NULL predicates and initialization safeguards,
including BOOLEAN TRUE/FALSE checks, remain active. The shared driver uses a
negative value index to skip only these logical-value assertions.

Metadata records one checked encode, four ordered case IDs, two rejections,
two successes and `target_lines=[]`. Source-fault mapping detects removal of
`ret = ret && bDecodingPatternMatches;` and the corresponding error assignment.
Both modes run ASan/UBSan and the shared acceptance, error, encoder-error,
input-write, consumption, view-count, field-offset, short-view, padding and
genuine missing-case faults.

Both public metrics include all twenty-six registered configurations;
`--unit '18-NULL/001.asn1#2'` selects this unit exactly. The incremental contract
is two added codec branch arms per mode, zero new codec statements and no
coverage losses across the fixed thirty-configuration cohort. The read-failure
arm remains unresolved. Existing positives, PUS initializer controls, strict
line gates and source-identity checks remain required. Measure this increment
against the committed campaign base, separately from positive-only pilot gains.

#### Sixteen-bit NULL pattern

`18-NULL/001.asn1#3` adds the fourteenth shared profile in legacy ACN and
ACN-v2. Checked encoding of `value=0` produces `AA FF`, returning TRUE/error
zero and consuming sixteen bits. All encode buffers, decode buffers and input
views are exactly two bytes. The cases execute in this order, each consuming
exactly sixteen bits:

| Case | Input | Result / error |
| --- | --- | --- |
| original | `AA FF` | TRUE / 0 |
| pattern0 | `AB FF` | FALSE / `ERR_ACN_DECODE_MYPDU` |
| pattern1 | `AA FE` | FALSE / `ERR_ACN_DECODE_MYPDU` |
| pattern2 | `00 00` | FALSE / `ERR_ACN_DECODE_MYPDU` |

Bounded field operations change only bit 7 for `pattern0`, bit 15 for
`pattern1`, and the full sixteen-bit pattern for `pattern2`. There is no
padding. The shared `None` value descriptor preserves NULL's absence of a
distinguishable logical output. Each case resets output, error and stream,
and checks exact result, error, consumption, input bytes and view count.
Transcript verification requires every case exactly once.

Metadata records one checked encode, four decodes, three rejections, one
success, the ordered case IDs above and `target_lines=[]`. Both modes use
the existing ASan/UBSan, unexpected-acceptance, wrong-error, decoder-writes,
wrong-consumption, wrong-view-count, wrong-encode-error, wrong-field-offset,
short-view and genuine missing-case checks. Source faults remove the pattern
rejection assignment or its error assignment. Padding and logical-value
faults are inapplicable to this profile.

Both public metrics include all twenty-eight registered configurations;
`--unit '18-NULL/001.asn1#3'` selects this unit exactly. The incremental contract
is two new codec branch arms per mode and zero new codec statements, measured
against the committed thirteen-unit baseline over the fixed thirty-configuration
cohort. The pattern read-failure arm remains unresolved. Existing positive
controls, PUS initializers, strict line gates and source-identity checks remain
required; positive-only pilot gains are separate historical measurements.

#### Constrained BIT STRING length

`08-BIT-STRING/001.asn1#1` adds the fifteenth shared profile in legacy ACN
and ACN-v2. Its checked seed has `nCount=16`, bytes `AB CD`, and encodes as
`7D 5E 68` (21 bits) in an exact three-byte buffer, with TRUE/error zero.
The first five wire bits encode `nCount - 1`; codes 20 and 31 exceed the
constrained range and fail before payload decoding or its error assignment.

| Case | Input bytes | Result/error | Consumed bits | Positive value |
|---|---|---|---|---|
| original | `7D 5E 68` | TRUE / 0 | 21 | 16 bits, `AB CD` |
| length-code20 | `A5 5E 68` | FALSE / 0 | 5 | inapplicable |
| length-code31 | `FD 5E 68` | FALSE / 0 | 5 | inapplicable |
| valid-shorter | `05 5E 68` | TRUE / 0 | 6 | one bit, `(arr[0] & 80) == 80` |
| padding | `7D 5E 69` | TRUE / 0 | 21 | 16 bits, `AB CD` |

Shared bounded operations replace the field at offset zero, width five,
with codes 20, 31 or zero. Padding flips only the final least significant
bit, one of three real trailing padding bits. Every case retains exactly
three backing/view bytes; the shorter message legitimately consumes six
bits. A field predicate can specify `{"mask": "0x80", "equals": "0x80"}`
to check meaningful bits only. Existing exact comparisons and NULL's `None`
descriptor retain their semantics; unused output bits and bytes are unchecked.

Metadata records one checked encode, five ordered decodes, two rejections,
three successes and `target_lines=[]`. Each case resets state and checks
exact result, error, consumption, view count, unchanged input and applicable
positive values. Transcript verification requires every named case once.

The public ASan/UBSan and fault suite includes all shared stream, length,
padding and value faults. A separate shorter-value fault clears its meaningful
high bit. Source-fault descriptors support explicit replacements as well as
the existing removal behavior: `missing-length-read` replaces exactly
`ret = BitStream_DecodeConstraintWholeNumber(pBitStrm, &nCount, 1, 20);`
with `nCount = 1; ret = TRUE;` in disposable test copies. This keeps the local
length initialized while testing detection of the bypassed read.
Disposable source-fault drivers probe the stream oracle before the original
positive suite, so an early round-trip failure cannot hide that oracle's
detection. Normal measurements retain the original positive-first execution.

Both public metrics include all thirty registered configurations;
`--unit '08-BIT-STRING/001.asn1#1'` selects this unit exactly. The incremental
contract is two new codec branch arms per mode and zero new statements against
the committed fourteen-unit baseline. All previous positive controls, faults,
PUS initializers and strict line gates remain required. Other uncovered
obligations remain unresolved; historical pilot gains are not new progress.

#### Parameterized PUS CHOICE

`15-PUS-ParameterPassing/001.asn1#1` uses the same profile driver in legacy ACN
and ACN-v2, bringing the registry to eight units (six enums and two CHOICE
profiles), sixteen configurations. Its checked `MySeq` seeds are green=3
(`1E 0A 20`, 20 bits, three bytes) and red=42 (`1E 14 0A 40`, 26 bits,
four bytes). Each seed executes original, `(31,10)`, `(30,11)`, `(50,10)`,
`(255,255)` and padding cases, in that order. Rejections replace only the two
eight-bit determinants at offsets 0 and 8; padding flips only the last byte's
lowest bit (`21` or `41`). Case IDs use the `seed0-` and `seed1-` prefixes.

Each mode adds two checked encodes, eight rejecting decodes and four successful
decodes. Rejections consume exactly 16 bits and return FALSE with
`ERR_ACN_DECODE_MYSEQ_COLORDATA`. Original/padding controls consume 20 or 26
bits, return TRUE/error zero, and reproduce the CHOICE kind and payload.
Exact-size buffers, byte preservation, stream counts and fresh output/error
state are enforced by the shared driver.

The shared encode contract defaults to error zero. This profile explicitly
requires zero in legacy mode and `ERR_ACN_DET_CONSISTENCY_MISMATCH` (203) in
v2, even though both checked encodes return TRUE. A `wrong-encode-error` fault
changes the successful call's error to 203 or zero respectively and must fail;
the existing compound CHOICE also retains its zero-error contract. The v2
decoder's exact private `COLORDATA` macro is resolved from its unique definition
in `sample1.c`, without substituting `COLORDATA_2` or editing codec/header files.

All shared faults apply, including real padding, field offset, short view,
positive value, omitted execution and removal of each source-mapped target
assignment. The target lines are 353–354 in legacy and 354–355 in v2. Both
public pilot metrics include this unit automatically; use
`--unit '15-PUS-ParameterPassing/001.asn1#1'` for exact selection. Its eight
original positives per mode remain intact. Incremental campaign measurements
must include the previously delivered compound CHOICE negatives in the base;
the public pilot's positive-only comparisons are not incremental campaign gains.

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

## Ada assertion contracts

The assertion regression builds with the ordinary strict flags plus `-gnata`.
It exercises short, empty and maximum fixed-width deduced lists, named aliases,
invalid counts/values and retained scalar/encoder capacity guards. It also
checks all 15 deferred patch variants at an end-of-stream cursor, including
wire bytes, surrounding bits, repeated values and invalid slot/cursor guards.
The original `24-DEDUCED-SIZE/004` and `/007` automatic suites and the
CONTAINING driver run with assertions in legacy/v2, normal/slim modes.

    docker run --rm --network none --entrypoint python3 asn1scc-coverage:local /opt/coverage/testAdaContracts.py

Only unaligned fixed-width deduced SEQUENCE OF decoders and their ordinary
aliases accept the smaller views through the adjusted contract. Other decoder
and encoder capacity contracts remain in force. This is a runtime assertion
regression, separate from statement coverage and SPARK proof. General bounded
decoding and variable-width deduced elements remain outside its scope.

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

#### Positive initializer controls in stream pilots

The PUS parameterized CHOICE profile also checks `ASN1SCC_COLOR_DATA_Initialize`:
it starts from a distinct valid red value, requires the initialized green value 1,
and validates that result. This positive control runs in both the public pilot's
baseline and mutation stages. Original ATCs are preserved; the extra control is
reported separately. The legacy line gate remains enabled for both gcov stages.

The paired public pilot delta therefore measures only stream-mutation gains.
A committed-base campaign comparison additionally reports newly added initializer
statements when this control is first introduced; those gains are distinct from
the selected decoder assignments. Fault checks detect an omitted control, wrong
initialized value and removed initializer assignment. Codecs and original ATCs
remain byte-identical; only the optional generated main driver is augmented.
