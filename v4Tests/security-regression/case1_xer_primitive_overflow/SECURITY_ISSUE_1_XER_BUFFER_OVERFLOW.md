# Security Issue: XER Primitive Decoder Buffer Overflow

## Summary

`Xer_DecodePrimitiveElement` copies XML element content into a caller-supplied buffer without bounds checking. Malformed XER input with oversized element content can overflow fixed-size stack buffers.

## Location

- **File:** `asn1crt/asn1crt_encoding_xer.c`
- **Function:** `Xer_DecodePrimitiveElement` (lines 248-330)
- **Exposed via:** `Xer_DecodeString`, `Xer_DecodeOctetString`, `Xer_DecodeObjectIdentifier`, etc.

## Affected Code

```c
while (c != '<')
{
    if (!GetNextChar(pByteStrm, &c))
        return FALSE;
    if (c == '<') {
        *pDecodedValue = 0x0;
        pDecodedValue++;
        break;
    }

    *pDecodedValue = c;   // No bounds check
    pDecodedValue++;
}
```

## Impact

- Callers use fixed buffers: 256 bytes (integers), 1024 bytes (octet strings, OIDs), 2048 bytes (bit strings)
- `Xer_DecodeString` passes through user buffer with unknown size
- Overflow corrupts stack, may enable code execution or crash

## Prerequisites

1. Application uses `-XER` flag during code generation
2. Application decodes XER/XML data from untrusted source
3. Attacker provides element content exceeding buffer size

## CVSS v3.1 Estimate

**Vector:** `AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:H`  
**Score:** 7.5 (High) - assuming XER decoder processes network input

If code execution is achievable: `AV:N/AC:H/PR:N/UI:N/S:U/C:H/I:H/A:H` = 8.1

*Note: Real-world impact depends on whether XER is used with untrusted input.*

## Suggested Fix

```diff
--- a/asn1crt/asn1crt_encoding_xer.c
+++ b/asn1crt/asn1crt_encoding_xer.c
@@ -245,8 +245,9 @@ flag Xer_EncodePrimitiveElement(ByteStream* pByteStrm, const char* elementTag, c
 }
 
 
-flag Xer_DecodePrimitiveElement(ByteStream* pByteStrm, const char* elementTag, char* pDecodedValue, int *pErrCode)
+flag Xer_DecodePrimitiveElement(ByteStream* pByteStrm, const char* elementTag, char* pDecodedValue, size_t maxLen, int *pErrCode)
 {
+	size_t written = 0;
 	Token t;
 	char c = 0x0;
 
@@ -288,12 +289,17 @@ flag Xer_DecodePrimitiveElement(ByteStream* pByteStrm, const char* elementTag, c
 	while (c != '<')
 	{
 		if (!GetNextChar(pByteStrm, &c))
 			return FALSE;
 		if (c == '<') {
 			*pDecodedValue = 0x0;
 			break;
 		}
+		if (written >= maxLen - 1) {
+			*pErrCode = ERR_INVALID_XML_FILE;
+			return FALSE;
+		}
 		*pDecodedValue = c;
 		pDecodedValue++;
+		written++;
 	}
 
 	PushBackChar(pByteStrm);
```

Update all callers to pass buffer size:

```diff
--- a/asn1crt/asn1crt_encoding_xer.c
+++ b/asn1crt/asn1crt_encoding_xer.c
@@ -707,7 +707,7 @@ flag Xer_DecodeInteger(ByteStream* pByteStrm, const char* elementTag, asn1SccSin
 {
 	char tmp[256];
 	memset(tmp, 0x0, sizeof(tmp));
-	if (!Xer_DecodePrimitiveElement(pByteStrm, elementTag, tmp, pErrCode))
+	if (!Xer_DecodePrimitiveElement(pByteStrm, elementTag, tmp, sizeof(tmp), pErrCode))
 		return FALSE;
 	*value = atoll(tmp);
 	return TRUE;
```

## Testing

1. Create XER input with element content > 2048 bytes
2. Decode using generated XER decoder
3. Verify decoder returns error instead of crashing

