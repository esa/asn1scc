# ICD regression tests

Automated tests for the ACN Interface Control Document generator (the
"new pipeline" that renders `<out>_new.html` and the `-icdRaw` JSON dump).
They guard against silent ICD regressions: a feature lands, the generated
document quietly loses a row or a table, and users notice months later.

## Layout

```
icd-tests/
  01-kitchen/            one test case per directory, named NN-<name>
    kitchen.asn1         input grammar(s)  (all *.asn1 in the dir are passed)
    kitchen.acn          ACN encoding spec (all *.acn in the dir are passed)
    kitchen.icd.json     committed golden: expected -icdRaw output
    xfails.txt           optional: expected-failure markers (see below)
  02-deduced/
  03-pdu10/
  04-choice13/
  05-pus15/
  06-presentwhen/
  07-mapping/
  08-units/
```

The initial five cases are the sample grammars used by the ICD analysis
(roadmap document): a kitchen-sink grammar covering ~15 ACN features, a
`size deduced` PDU, the `acnv2/10` CONTAINING PDU with an empty header
SEQUENCE, the issue-#379 nested CHOICE with present-when determinants, and a
PUS parameter-passing CHOICE with parameterized types. `06-presentwhen`
(from `test-cases/acn/21-PresentWhenExpression/001`) covers an optional
field governed by a `present-when` boolean *expression*, whose condition
must render in ACN syntax in the Present column. `07-mapping` covers ACN
`mapping-function`s: mapped INTEGER types with and without a module, a mapped
SEQUENCE OF element, and a size-0 ACN length determinant whose value the
mapping-function supplies at run time. `08-units` covers units of measure (`--{deg}--`) on
references (GitHub #433): the unit written at the use site replaces the
referenced type's (`a1 Angle --{deg}--` with `Angle ... --{rad}--`), also on a
CHOICE alternative, a type assignment that is a reference and the elements of
a SEQUENCE OF, and never goes on the Length row of a variable-size string.

## Running

```bash
cd v4Tests
./scripts/runIcdTests.py                  # all test cases, fail-fast
./scripts/runIcdTests.py -t kitchen       # only dirs whose name contains "kitchen"
./scripts/runIcdTests.py --update-goldens # regenerate the golden .icd.json files
./scripts/runIcdTests.py --keep-temp      # keep the per-test temp output dirs
```

The compiler defaults to `../asn1scc/bin/Debug/net10.0/asn1scc` (same
convention as `runTests.py`); override with `--compiler`. Python 3; the
optional `openpyxl` package lets the `generator` check also render Excel
(without it that part is skipped with a note; `--require-excel`, used by CI and
`local-build.sh`, makes it a failure). Like `runTests.py`, the run stops at the first
unexpected failure and leaves that test's temporary output directory in
place for inspection.

## What is checked

For each test case the grammar is compiled once with C output, `-icdAcn`,
`-icdRaw` and `-x` (AST xml) into a temporary directory, then:

| check          | meaning |
|----------------|---------|
| `golden`         | the `-icdRaw` JSON is byte-identical to the committed golden (unified diff on mismatch) |
| `json-structure` | `navOrder` matches the emitted tables; every type-reference row points at an emitted table |
| `nav-labels`     | no two tables render the same nav-pane label (display name) |
| `presence-mask`  | the uPER presence-mask row width equals the number of OPTIONAL children *without* an ACN `present-when` (computed from the AST xml) |
| `choice-alts`    | every CHOICE table lists exactly the non-AlwaysAbsent alternatives of the ASN.1 CHOICE (from the AST xml) |
| `c-macros`       | each TAS table's max bytes equals the `<TAS>_REQUIRED_BYTES_FOR_ACN_ENCODING` macro in the generated C header, and BYTES == ceil(BITS/8) |
| `tas-coverage`   | every TAS that received `REQUIRED_*` macros in the C headers has an ICD table (catches zero-bit types vanishing from the document) |
| `html-anchors`   | every `href="#..."` in the `_new.html` resolves to an anchor in the same file |
| `mapping-fns`    | the ACN mapping-functions of the AST xml (`mapping-function` attribute) are exactly those named by the `mapping-function <fn>: ...` row notes of the `-icdRaw` JSON, and the note says "occupies no bits on the wire" exactly on the rows of size 0 |
| `generator`      | `contrib/icd-generator.py` renders the case without error: HTML from the grammar (no dead `#` link, every mapping-function shown), HTML from the `-icdRaw` dump, and Excel when `openpyxl` is installed |
| `units`          | every value row (`FieldRow`, `SubItemRow`) has the unit of measure of its ASN.1 node in the AST xml (node id = table `usagePath` + field name; the outer node of a reference, i.e. the unit written at the use site, first), or none when the node has none; length / presence / choice-index / padding rows have no unit |
| `v2-parity`      | opt-in (see below): the `-icdRaw` JSON produced with `--acn-v2` is byte-identical to the legacy one, so closure conversion / deferred patching does not leak into the ICD (roadmap B8) |

The invariant checks (everything except `golden`) are grammar-independent:
they hold for any grammar, so new test cases get them for free.

### `v2-parity` (opt-in)

`--acn-v2` (deferred patching + closure conversion) is a C-only, experimental
encoding backend.  For a grammar that compiles in both modes, its ICD **must**
match legacy mode: the deferred transform is an implementation detail that the
document should never expose. A test dir opts in by containing an empty marker
file `acn-v2-parity.marker`; the runner then recompiles the grammar with
`--acn-v2 -icdRaw` and diffs the JSON against the legacy one. Dirs without the
marker skip the check (it is not reported). `03-pdu10` carries the marker: its
empty `Header` SEQUENCE (cross-scope size/reference determinants) and
`CONTAINING PayloadData` are exactly where closure conversion used to leak the
usage-path name, drop determinant comments, and split a merged table.

## Expected failures (xfails.txt)

An invariant may fail because of a documented, not-yet-fixed generator bug
(see the ICD analysis roadmap, correctness findings R1-R8). No such bug is
open at the moment — all previously marked expected failures have been fixed:

- R6 (dead colorizer anchors for multi-hash names, duplicate ACN anchors) was
  fixed by roadmap B5: the definition site anchors every table of the name and
  references render as plain links.
- R7 (duplicate nav labels from in-context tables reusing the bare TAS name)
  was fixed by roadmap B4: byte-identical specializations merge into one table
  and remaining duplicates are disambiguated with their usage context.

So that the suite is green while those bugs exist, a test dir may contain an
`xfails.txt` with one marker per line:

```
<check-name> <roadmap-id> [free-text reason]
```

A marked check that fails is reported as `XFAIL` and does not fail the run.
A marked check that *passes* is reported as `XPASS` and **fails the run**:
whoever fixes the bug must delete the marker and update the golden in the
same commit — expected failures cannot go stale silently.

## Adding a test case

1. Create `NN-<name>/` with the `.asn1` and `.acn` inputs (`NN` keeps the
   run order deterministic; `<name>` names the golden file).
2. Run `./scripts/runIcdTests.py --update-goldens -t <name>` to create
   `<name>.icd.json`, then a plain run to see which invariants fail.
3. Eyeball the generated JSON/HTML before committing the golden — the golden
   captures *current* behavior, which is only useful if a human confirmed it
   is the *intended* behavior (or marked the known deviations).
4. Add `xfails.txt` markers, each annotated with the roadmap id it waits on.

## Updating goldens

Golden diffs are only legitimate when a compiler change *intends* to change
the ICD. Regenerate with `--update-goldens`, inspect the diff (`git diff`),
and justify every changed hunk in the commit message.
