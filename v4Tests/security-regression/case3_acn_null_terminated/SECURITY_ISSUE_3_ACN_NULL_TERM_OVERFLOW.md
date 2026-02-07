# Security Issue: ACN Null-Terminated Decoder Buffer Overflow

## Summary

ACN null-terminated decoders allocate a fixed 10-byte buffer for the termination pattern, but the read loop uses the unclamped `null_character_size` parameter. Termination patterns longer than 10 bytes overflow the stack buffer.

## Location

- **File:** `asn1crt/asn1crt_encoding_acn.c`
- **Functions:**
  - `Acn_Dec_String_Ascii_Null_Terminated_mult` (lines 1320-1345)
  - `Acn_Dec_UInt_ASCII_VarSize_NullTerminated` (lines 849-879)
  - `Acn_Dec_SInt_ASCII_VarSize_NullTerminated` (lines 882-901)

## Affected Code

```c
flag Acn_Dec_String_Ascii_Null_Terminated_mult(BitStream* pBitStrm, asn1SccSint max, 
    const byte null_character[], size_t null_character_size, char* strVal)
{
    byte tmp[10];                                              // Fixed 10-byte buffer
    size_t sz = null_character_size < 10 ? null_character_size : 10;  // sz is clamped
    memset(tmp, 0x0, 10);
    memset(strVal, 0x0, (size_t)max + 1);
    
    for (int j = 0; j < (int)null_character_size; j++) {       // Loop uses UNCLAMPED size!
        if (!BitStream_ReadByte(pBitStrm, &(tmp[j])))          // Overflow if size > 10
            return FALSE;
    }
    // ...
}
```

## Impact

- `sz` is clamped to 10 for `memcmp`, but read loop uses raw `null_character_size`
- Termination patterns > 10 bytes write past `tmp[10]` on stack
- Stack corruption on first decode attempt with oversized pattern
- Pattern size is compile-time constant from ACN schema

## Prerequisites

1. ACN schema specifies `termination-pattern` longer than 10 bytes
2. No compile-time validation rejects patterns > 10 bytes
3. Application decodes data using generated decoder

**Note:** This requires a malformed/unusual ACN schema. Typical patterns are 1-3 bytes.

## CVSS v3.1 Estimate

**Vector:** `AV:L/AC:L/PR:N/UI:N/S:U/C:N/I:L/A:H`  
**Score:** 6.1 (Medium)

- Attack Vector: Local (requires control of ACN schema at compile time)
- Not directly exploitable from network data alone
- Availability: High (crash on any decode)
- Integrity: Low (stack corruption)

*Lower severity because exploitation requires attacker-controlled schema, not just runtime data.*

## Suggested Fix

### Option A: Use clamped size consistently (minimal change)

```diff
--- a/asn1crt/asn1crt_encoding_acn.c
+++ b/asn1crt/asn1crt_encoding_acn.c
@@ -1324,7 +1324,7 @@ flag Acn_Dec_String_Ascii_Null_Terminated_mult(BitStream* pBitStrm, asn1SccSint
 	memset(tmp, 0x0, 10);
 	memset(strVal, 0x0, (size_t)max + 1);
 	//read null_character_size characters into the tmp buffer
-	for (int j = 0; j < (int)null_character_size; j++) {
+	for (int j = 0; j < (int)sz; j++) {
 		if (!BitStream_ReadByte(pBitStrm, &(tmp[j])))
 			return FALSE;
 	}
@@ -1333,9 +1333,9 @@ flag Acn_Dec_String_Ascii_Null_Terminated_mult(BitStream* pBitStrm, asn1SccSint
 	while (i <= max && (memcmp(null_character, tmp, sz) != 0)) {
 		strVal[i] = tmp[0];
 		i++;
-		for (int j = 0; j < (int)null_character_size - 1; j++)
+		for (int j = 0; j < (int)sz - 1; j++)
 			tmp[j] = tmp[j + 1];
-		if (!BitStream_ReadByte(pBitStrm, &(tmp[null_character_size - 1])))
+		if (!BitStream_ReadByte(pBitStrm, &(tmp[sz - 1])))
 			return FALSE;
 	}
```

Apply same fix to `Acn_Dec_UInt_ASCII_VarSize_NullTerminated` (lines 858, 865, 867).

### Option B: Add compile-time validation (defense in depth)

In `FrontEndAst/AcnCreateFromAntlr.fs`, add size check:

```fsharp
| Some bitPattern ->
    match bitPattern.Value.Length % 8 <> 0 with
    | true  -> raise(SemanticError(bitPattern.Location, "termination-pattern must be a sequence of bytes"))
    | false ->
        let ba = bitStringValueToByteArray bitPattern |> Seq.toList
        if ba.Length > 10 then
            raise(SemanticError(bitPattern.Location, "termination-pattern cannot exceed 10 bytes"))
        Some(AcnGenericTypes.StrNullTerminated ba)
```

## Testing

1. Create ACN schema with `termination-pattern '0102030405060708090A0B0C'H` (12 bytes)
2. Attempt to compile with asn1scc
3. With Option A: Decoder silently uses first 10 bytes (behavior change)
4. With Option B: Compiler rejects schema with clear error

