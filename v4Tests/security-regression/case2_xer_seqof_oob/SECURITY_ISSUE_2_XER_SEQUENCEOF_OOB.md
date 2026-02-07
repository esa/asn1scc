# Security Issue: XER SEQUENCE OF Out-of-Bounds Write

## Summary

The XER SEQUENCE OF decoder template writes elements to `arr[sI]` inside a loop without checking if `sI` has reached `nSizeMax`. The bounds check occurs after the loop completes, allowing out-of-bounds writes on malformed input.

## Location

- **File:** `StgC/xer_c.stg` (C backend)
- **Template:** `SequenceOf_decode` (lines 258-282)
- **Also affected:** `StgAda/xer_a.stg` (lines 279-299)

## Affected Code

```c
while(ret && !Xer_NextEndElementIs(pByteStrm, <sTag>))
{
    <sChildBody>        // Writes to arr[sI] - NO BOUNDS CHECK
    <sI>++;
    <if(!bFixedSize)><p><sAcc>nCount++;<endif>
}
// ...
*pErrCode = (ret && <sI> == <nSizeMax>) ? 0 : <sErrCode>;  // Check is TOO LATE
```

## Impact

- Attacker provides XER with more child elements than `nSizeMax`
- Each extra element writes past the end of the fixed-size array
- Stack/heap corruption depending on array allocation
- Bounds check after loop cannot prevent the overflow

## Prerequisites

1. Application uses `-XER` flag during code generation
2. Application decodes XER data from untrusted source
3. ASN.1 schema contains a `SEQUENCE OF` with size constraint
4. Attacker provides more elements than the constraint allows

## CVSS v3.1 Estimate

**Vector:** `AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:L/A:H`  
**Score:** 8.2 (High)

- Availability: High (crash via corruption)
- Integrity: Low (memory corruption, but controlled exploitation is harder)

*Note: Real-world impact depends on whether XER is used with untrusted input.*

## Suggested Fix

### C Template (StgC/xer_c.stg)

```diff
--- a/StgC/xer_c.stg
+++ b/StgC/xer_c.stg
@@ -258,9 +258,13 @@ SequenceOf_decode(p, sAcc, sTag, nLevel, sI, nSizeMax, sChildBody, bFixedSize, s
 /* SEQUENCE OF Decode*/
 ret = Xer_DecodeComplexElementStart(pByteStrm, <sTag>, NULL, pErrCode);
 if (ret) {
     <if(!bFixedSize)><p><sAcc>nCount = 0;<endif>
     <sI> = 0;
     while(ret && !Xer_NextEndElementIs(pByteStrm, <sTag>))
     {
+        if (<sI> >= <nSizeMax>) {
+            ret = FALSE;
+            *pErrCode = <sErrCode>;
+            break;
+        }
 	    <sChildBody>
 	    <sI>++;
 	    <if(!bFixedSize)><p><sAcc>nCount++;<endif>
     }
```

### Ada Template (StgAda/xer_a.stg)

```diff
--- a/StgAda/xer_a.stg
+++ b/StgAda/xer_a.stg
@@ -283,6 +283,11 @@ if ret.Success then
     <sI> := 1;
     while ret.Success and not adaasn1rtl.encoding.xer.Xer_NextEndElementIs(bs, <sTag>) loop
+        if <sI> > <nSizeMax> then
+            ret.Success := False;
+            ret.ErrorCode := <sErrCode>;
+            exit;
+        end if;
 	    <sChildBody>
 	    <sI> := <sI> + 1;
     end loop;
```

## Testing

1. Define `TestSeq ::= SEQUENCE (SIZE(1..5)) OF INTEGER`
2. Generate XER decoder with `-XER`
3. Provide XER input with 10 `<item>` elements
4. Verify decoder returns error after 5th element instead of writing past array bounds

