# Bug: ACN accepts duplicate CHOICE child names without error

## Description

When an ACN spec references the same CHOICE child name twice (e.g., `case1` instead of `case1`/`case2`), the compiler does not report an error. Instead, it silently generates incorrect code — one child gets inlined encoding while the other gets a plain function call, leading to runtime test failures.

## Reproduction

```acn
PDU [] {
   payload [] {
      case1 [] {
         int-field [encoding twos-complement, size 32],
         buffer [size  hdr.buffers-length]
      },
      case1 [] {                        -- <-- should be case2, not case1
         int-field [encoding twos-complement, size 32],
         buffer [size  hdr.buffers-length]
      }
   }
}
```

## Expected behavior

The compiler should report an error: duplicate CHOICE child name `case1` in ACN specification.

## Actual behavior

No error reported. Code generation proceeds, producing code where `case2` does not get the ACN attributes applied. Auto-generated test cases for PDU fail at runtime (encoding failure for case2).

## Discovered

During ACN v2 refactoring work, test case `acnv2/03/`.
