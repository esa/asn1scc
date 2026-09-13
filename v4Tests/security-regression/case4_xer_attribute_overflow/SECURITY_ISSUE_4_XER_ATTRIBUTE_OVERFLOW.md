# Security Issue: XER `AddAttribute()` Unbounded Stack Write

Reported to ESA through ESACERT (ticket #74508), September 2026.

## Summary

`Xer_DecodeAttributes()` parses the attributes of an XML start tag in an unbounded loop
and stores each one with `AddAttribute()`, whose only capacity guard was an `assert()`.
With `-DNDEBUG` (the normal setting for release and flight builds) the assert is compiled
out and attributes beyond the 20th are written past the end of the fixed-size
`XmlAttributeArray` with attacker-controlled bytes (up to 99 characters each, the limit of
the tokenizer). A second defect let attribute names of up to 99 characters overflow the
50-byte `Name` field into the adjacent `Value` field even when the array was not full.

## Location

- **File:** `asn1crt/asn1crt_encoding_xer.c`
- **Functions:** `AddAttribute()`, `Xer_DecodeAttributes()`
- **Supporting header:** `asn1crt/asn1crt.h` (`XmlAttributeArray` with `attrs[20]`,
  `XmlAttribute` with `Name[50]` / `Value[100]`)

## Affected Code

```c
void AddAttribute(XmlAttributeArray* pAttrArray, const char* attr, const char* val)
{
	assert(((unsigned)pAttrArray->nCount) < (sizeof(XmlAttributeArray) - sizeof(int)) / sizeof(XmlAttribute));

	strcpy(pAttrArray->attrs[pAttrArray->nCount].Name, attr);   /* Name[50] <- token of up to 99 chars */
	strcpy(pAttrArray->attrs[pAttrArray->nCount].Value, val);
	pAttrArray->nCount++;
}

flag Xer_DecodeAttributes(ByteStream* pByteStrm, XmlAttributeArray* pAttrs, int *pErrCode)
{
	...
	while (LA(pByteStrm).TokenID != '>') {          /* no cap on the number of attributes */
		...
		AddAttribute(pAttrs, t1.Value, t2.Value);   /* return value: none */
	}
	return TRUE;
}
```

## Impact

Two behaviours were reachable, depending on the caller:

1. **Generated decoders.** The code emitted by asn1scc always calls
   `Xer_DecodeComplexElementStart(pByteStrm, tag, NULL, pErrCode)`, so `pAttrs` is `NULL`.
   Any attribute on a complex element made `AddAttribute()` dereference `NULL`: a crash,
   i.e. denial of service, on every attribute-bearing input.
2. **Callers that supply their own `XmlAttributeArray`** (hand-written code calling the
   runtime directly, the scenario of the report's proof of concept). More than 20
   attributes produce a linear write of attacker-controlled bytes past the array; if the
   array lives on the stack this is a classic stack smash and a potential code-execution
   primitive. A name longer than 49 characters corrupts the neighbouring `Value` field
   (intra-object overflow, not detectable by AddressSanitizer).

## Prerequisites

1. Application generated with the `-XER` flag and built with the C runtime.
2. Application decodes XER/XML data from an untrusted source.
3. For the out-of-bounds write (as opposed to the NULL-dereference crash) the application
   must call the runtime with its own attribute array.

Ada and Scala runtimes are not affected: their XER decoders do not parse attributes at all
and reject anything between the element name and `>`.

## Fix

`AddAttribute()` became a checked, bounded function that fails closed, and
`Xer_DecodeAttributes()` aborts the decode when an attribute cannot be stored:

```c
#define MAX_NUM_OF_XML_ATTRIBUTES	((int)(sizeof(((XmlAttributeArray*)0)->attrs) / sizeof(((XmlAttributeArray*)0)->attrs[0])))

flag AddAttribute(XmlAttributeArray* pAttrArray, const char* attr, const char* val)
{
	size_t attrLen;
	size_t valLen;

	if (pAttrArray == NULL)
		return FALSE;
	if (pAttrArray->nCount < 0 || pAttrArray->nCount >= MAX_NUM_OF_XML_ATTRIBUTES)
		return FALSE;

	attrLen = strlen(attr);
	valLen = strlen(val);
	if (attrLen >= sizeof(pAttrArray->attrs[0].Name))
		return FALSE;
	if (valLen >= sizeof(pAttrArray->attrs[0].Value))
		return FALSE;

	memcpy(pAttrArray->attrs[pAttrArray->nCount].Name, attr, attrLen + 1);
	memcpy(pAttrArray->attrs[pAttrArray->nCount].Value, val, valLen + 1);
	pAttrArray->nCount++;
	return TRUE;
}
```

```c
		if (!AddAttribute(pAttrs, t1.Value, t2.Value)) {
			*pErrCode = ERR_INVALID_XML_FILE;
			return FALSE;
		}
```

The `NULL` check also turns the previous crash of generated decoders into a clean
`ERR_INVALID_XML_FILE` result: attributes are rejected when the caller provided no storage
for them.

## Testing

`reproduce_issue.sh` in this directory builds the generated decoder and the runtime with
`-DNDEBUG` and AddressSanitizer and checks that:

- a well-formed document still decodes and a 20-attribute tag still fits (positive controls);
- one or forty attributes on a generated decoder return `FALSE` instead of crashing;
- forty attributes, a 90-character name, or a `NULL` array passed to `Xer_DecodeAttributes()`
  return `FALSE` with `nCount` never above 20 and no sanitizer report.
