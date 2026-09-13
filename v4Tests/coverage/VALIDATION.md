# Phase 0b implementation and validation — 2026-09-07

The collector, container and local Docker validation are implemented in the
working tree. A manual-only GitHub workflow is provided; it has not been
dispatched remotely. No generated-code behavior or Phase-1 test generation was
changed.

## Reproducible environment

- Final image: asn1scc-coverage:phase0b
- Image ID: sha256:84a42040febc5ecf28d3b7a5705ff8024e16ceb39536e2a40e4d695f88702f48
- Base image digest: sha256:e1ffd2a92ae84c1291bc1b6887501f8af98e6331e7af6d4c8d37168c5e87a64c
- .NET SDK 10.0.400; GCC/gcov 13.3.0; Ubuntu GNAT 13.
- Build and runtime user: UID/GID 10001/10001.
- Compiler source revision: e0736817f2b1470252c78dcea196b69147bcf9e0.
- Final supplied-source snapshot:
  dad82ebd74d3a1d617961fca4ac1e67caeffd9f61a8e7391354d7564b5934656.
- Compiler-distribution hash:
  951749a809aea5fd33ef2e005eb85cfdcd837ca949e26408ada819869be8cb93.

The full-corpus and mode runs used the container-built compiler with successive
collector revisions mounted read-only during development. The final image then
embedded the corrected collector and passed the standalone checks below without
mounts. The compiler distribution hashes match across these runs; the final
embedded collector hash matches the working-tree script. Measurement containers
were run with --network none. No host .NET, GCC, GNAT or gcov was used.

## Results

| Run | Result | Notes |
|---|---|---|
| Full C, uPER + legacy ACN | 305/305 units successful | All 44 NOCOVERAGE units included; nine NO_AUTOMATIC_TEST_CASES accounted for separately |
| Historical subset within full C | 259/259 exact matches | Individual input hashes and line/branch counts match the checked-in reference |
| C/ACN-v2 pilot | 12/12 successful | Same explicit pilot IDs as Ada |
| Ada/legacy pilot | 12/12 successful | Source specs and compiler-generated initialization code retained |
| Ada/ACN-v2 pilot | 11 successful, one runtime failure | 24-DEDUCED-SIZE/004.asn1#1; recorded as failure, not an exclusion |
| Final image collector tests | 20/20 successful | Failure/timeout/profile/empty-source/legacy-grep/reference tests |
| Final image C pilot without mounts | 12/12 successful | Validates final packaging |
| Final image C, word-size 4 + slim + ACN-only | One successful BOOLEAN unit | Smoke coverage of these optional flags; not a full 32-bit/slim matrix |
| Final image Ada/v2 known failure without mounts | Same unit fails | Nonzero exit, seven generated tests fail, stdout details retained |

Successful full-C generated-code totals:

| Metric | Value |
|---|---:|
| Raw lines hit/total | 67,052 / 71,435 |
| Raw branch arms taken/total | 32,383 / 57,108 |
| Basic blocks hit/total | 72,300 / 82,344 |
| Executed line occurrences with unexecuted blocks | 8,164 |
| Legacy-adjusted lines hit/total | 62,707 / 63,444 |
| Legacy line misses in non-exempt units | 0 |
| Legacy line misses in NOCOVERAGE units | 737 |
| Justified branch exclusions applied | 0 |

These are instance-weighted generated-code counts, not unique repository-source
coverage or statement coverage. Failed units do not contribute to successful
totals. Do not compare a failed-mode percentage to another mode as if their
successful cohorts were identical.

## Actionable coverage gaps

All 737 legacy-gate misses are in the following exempt units:

| Unit | Misses |
|---|---:|
| 14-RealCases/NPAL.asn1#1 | 504 |
| 15-PUS-ParameterPassing/004.asn1#1 | 114 |
| 16-mantis/0000806.asn1#1 | 46 |
| 18-NULL/001.asn1#1 | 20 |
| 09-CHOICE/013.asn1#1 | 12 |
| 14-RealCases/05.asn1#1 | 8 |
| 18-NULL/001.asn1#2 | 8 |
| 18-NULL/001.asn1#3 | 8 |
| 16-mantis/0000807.asn1#1 | 6 |
| 16-mantis/0000807b.asn1#1 | 6 |
| 16-mantis/0000774-DataTypesSimulink.asn1#1 | 2 |
| 16-mantis/000724-DataTypesSimulink.asn1#1 | 2 |
| 10-SEQEUENCE/empty_seq.asn1#1 | 1 |

NPAL and PUS account for 618 of the 737 misses. Classify their actual source
locations before choosing positive-case synthesis, negative tests or proof of
unreachability; the counts alone do not establish the cause.

## Open Ada/ACN-v2 runtime finding

Reproducer:

    docker run --name coverage-ada-v2-repro --network none asn1scc-coverage:phase0b --outdir /results --cohort pilot --filter 24-DEDUCED-SIZE/004 --language Ada --acn-v2
    docker cp coverage-ada-v2-repro:/results ./coverage-results

Build succeeds and the test program exits 1. Of 32 generated tests:

- ACN_000023, ACN_000024, ACN_000026 and ACN_000027 report different encoded/
  decoded values.
- ACN_000030, ACN_000031 and ACN_000032 fail decoding.
- All 16 uPER tests pass.

The C/v2 and Ada/legacy runs of this unit pass. The collector does not mask this
failure with the file's NOCOVERAGE marker. Root-cause analysis and a compiler
fix are separate work; no compiler fix was attempted here. The manual CI job
is expected to report failure while this runtime finding remains open, while
still uploading reports for all configurations.

## Measurement safeguards verified

Empty translation units (no executable lines/functions) and empty GNAT source
mappings are recorded with zero counts; they are not treated as missing
coverage. Missing profiles or reports for actual code remain errors. Literal
legacy grep false positives caused by hashes in decorative comments are
recorded separately from checks of the gcov execution-count column.

Branch IDs are exact-source/configuration/translation-unit coordinates.
Compare them only with compatible recorded toolchains; they are not semantic
IDs portable across compiler versions or source rewrites. Every untaken branch
remains unresolved; no B6/B8 category or legacy marker creates a proof.

## Evidence and remaining scope

Local summaries/manifests/reports are under tmp/coverage-phase0b/. Full unit
work directories, instrumented objects and raw gcov remain in the named Docker
containers (they were intentionally not removed):

- coverage-phase0b-all-c
- coverage-phase0b-c-v2
- coverage-phase0b-ada-pilot2
- coverage-phase0b-ada-v2
- coverage-phase0b-final-c
- coverage-phase0b-final-ada-v2
- coverage-phase0b-final-c32

Use docker cp to export additional artifacts before removing these containers.
The initial incomplete smoke runs are also retained for debugging.

The separate source-statement instrumentation lane is selected as GNATcoverage
at --level=stmt, but is not provisioned in this image. Phase 0b currently
provides verified line/basic-block/branch measurement and explicit scope.
Semantic field/goal IDs, reachability proofs and Phase-1 negative tests remain
later work. Full Ada/v2, word-size and slim matrices beyond the stated pilot
have not been measured.
