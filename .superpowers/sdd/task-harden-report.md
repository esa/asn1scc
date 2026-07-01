# XER Hardening Pass Report

## Status: DONE

## Step 1 — New In-Repo XER Grammar

Created `v4Tests/test-cases/xer/01-basic/003-nested.asn1` with:
- `MyPDU` top-level SEQUENCE containing:
  - `a INTEGER` (scalar)
  - `inner` — inline anonymous SEQUENCE with 3 children including `deep`, itself an inline anonymous SEQUENCE
  - `list` — SEQUENCE OF inline anonymous SEQUENCE
  - `pick` — CHOICE whose `two` alternative is an inline anonymous SEQUENCE

## Step 2 — Empirical cls Save/Restore Test

The new grammar **passed immediately as-is** (1 passed in 0.04s). No saver-naming fix was required.

### Analysis of the cls clobbering pattern

Inspecting the generated `TEST_CASE.py` for `MyPDU._XERdecode` confirmed the
reviewer's flagged hazard is real but benign in the current pattern:

When `sChildContent` for a mandatory child is itself an inlined anonymous
SEQUENCE, the inner block writes `_xer_saved_cls = cls` which overwrites the
outer block's save. On the outer `cls = _xer_saved_cls` restore, `cls` receives
the wrong type (e.g. an inner child type rather than the outer SEQUENCE type).

This is safe because the restored corrupted `cls` is never used for object
construction: the very next sibling child block immediately executes
`_xer_saved_cls = cls; cls = <correct_child_type>` before any `cls(...)`
construction call. The corrupted restore value only propagates into the next
sibling's save (which is immediately overwritten), and after the last child,
`cls` is not used again before the function returns.

The invariant is documented via comments added in Step 3.

## Step 3 — Cleanups

### Dead binding removed
`BackendAst/EncodeDecodeTestCase.fs` line 233: removed `let encode = lm.atc.Codec_Encode`
in `_createXerEncDecFunction`. The binding was superseded by `encodeXER` (line 232)
and was never referenced within the function body.

### Comments added to xer_python.stg
- `SequenceOf_decode`: added inline invariant comment explaining the `_seqof_cls`
  save/restore semantics.
- `Sequence_mandatory_child_decode`: added inline invariant comment explaining why
  `_xer_saved_cls` clobbering by nested inlined child blocks is safe.

Note: StringTemplate angle-bracket syntax requires avoiding `<name>` in Python
comments inside template bodies; comments use plain text descriptions instead.

## Step 4 — Verification Results

### Build
`rm StgPython/*.stg.fs && dotnet build asn1scc 2>&1 | tail -3` → **0 errors**

### XER test suite (v4Tests --xer)
```
Test run ended succesfully. Number of test cases run : 3
```
All 3 XER grammars pass (001.asn1, 002-empty-string.asn1, 003-nested.asn1).

### Testlib XER
`uvx --python 3.11 pytest -q asn1src/` → **700 passed in 0.90s**

### v4Tests regression (ws=4)
`regression -l python -ws 4 -s false -p 12` → **338/338 OK**, exit 0

## Files Changed

- `v4Tests/test-cases/xer/01-basic/003-nested.asn1` — new grammar (added)
- `StgPython/xer_python.stg` — invariant comments in SequenceOf_decode and Sequence_mandatory_child_decode
- `BackendAst/EncodeDecodeTestCase.fs` — removed dead `let encode` binding at line 233
