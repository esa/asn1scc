# ACN Protocol Feature Addition: Support of decuced size types for PUS Protocol

## Abstract

This document proposes a new feature for the ACN protocol in asn1scc: `deduced-size` support for certain ASN.1 types. This feature will allow the number of elements in these types to be determined by the number of encoded bytes, a requirement in many use-cases, including those involving the Packet Utilization Standard (PUS) protocol commonly used in space applications.


## Current Limitations

The ACN protocol in asn1scc allows the protocol designer to control the number of elements in a SEQUENCE OF or OCTET STRING or BIT STRING or IA5String in the following ways:

1. **Fixed size elements**: In this case, the number of elements in the SEQUENCE OF is predefined between then encoder (sender) and decoder (receiver). This is modeled in ASN.1 using a fixed size constraint. E.g. `MyFixSeqOf ::= SEQUENCE (SIZE (100)) OF INTEGER`. `MyFixSeqOf` will always contain 100 INTEGERS and hence there is no need to provide any additional information about the number of elements in the array.

2. **Variable size with an embedded length determinant field**: E.g. `MyVarSeqOf ::= SEQUENCE (SIZE (0 .. 100)) OF INTEGER`. In this case `MyVarSeqOf` can contain from 0 to 100 elements. The sender encodes the number of the elements in a fixed length field, the length determinant, just before the first element of the SEQUENCE OF. Since the number of the elements is from 0 .. 100, the size of the embedded length determinant is 7 bits (since a 7 bits unsigned integer can hold values from 0 to 127).

3. **Variable size with an external length determinant field**: This is similar to the above case. The difference is that ACN allows the use of an external integer field as length determinant. The advantage of this approach is that it allows to specify the position and the size of the length determinant field.

4. **Using a termination bit pattern**: In this case, the encoder specifies the end of the elements by appending at the end a predefined  termination bit pattern (e.g. 0xFFFF).

However, there are cases where the number of elements in a SEQUENCE OF (or OCTET STRING, BIT STRING or IA5String) is not fixed, is not determined by a length determinant nor by a termination bit pattern. The number of elements is expected to be determined by the number of the encoded bytes. This feature is currently not supported in asn1scc's implementation of the ACN protocol, limiting its usability in certain scenarios where the protocol size is determined by the number of encoded bytes.


## Proposed Feature

The proposed feature aims to extend the ACN protocol in asn1scc with a new ACN property: `deduced-size`. This new property can be used in SEQUENCE OF, OCTET STRING, or IA5Strings (but not in BIT STRINGS as explained in the limitations section) to indicate that the number of elements is determined (deduced) by the number of octets in the underlying encoded data. 

Example usage:

For the ASN.1 type 
```ASN.1
	MySequenceOf ::= SEQUENCE (SIZE (0..100)) OF SOME-TYPE
```
the ACN notation will be 
```ACN
	MySequenceOf [deduced-size]
```

This notation indicates that the number of INTEGERS in `MySequenceOf` is not fixed, nor determined by an embedded or external length determinant field, nor a termination bit pattern, but deduced from the number of encoded bytes.


### Technical Description


When a SEQUENCE OF (or OCTET STRING or IA5String) is marked as `deduced-size`, the decoding size in bytes of must be passed to the generated decoding function. This necessitates the addition of a new parameter to the decoding function prototype in the C and Ada code.

Example usage:

For the ASN.1 type `MySequenceOf ::= SEQUENCE (SIZE (0..100)) OF SOME-TYPE` and the ACN notation `MySequenceOf [deduced-size]`, the prototype of the decoding C function will be:

```c
flag MySequenceOf_ACN_Decode(MySequenceOf* pVal, BitStream* pBitStrm, int* pErrCode, const int totalEncodedBytes);
```

Note that a new parameter `const int totalEncodedBytes` has been added to the decoding function.

The implementation of the above function will look like the following:

```c
flag MySequenceOf_ACN_Decode(MySequenceOf* pVal, BitStream* pBitStrm, int* pErrCode, const int totalEncodedBytes) {
  pVal->nCount = 0;
  int availableBits = totalEncodedBytes * 8;

  // SOME_TYPE_LA() is a function that returns true if decoding is possible based on the number of availables bits.
  // It doesn't change the state of the bit stream.
  while (pVal->nCount <= 100 && SOME_TYPE_LA(pStream, availableBits)) {  
    int startPos = pStream->currentPosition;
    SOME_TYPE_ACN_decode(pStream, &pVal->arr[pVal->nCount]);
    int endPos = pStream->currentPosition;
    pVal->nCount++;
    availableBits -= (endPos - startPos);
  }
  // At this point the availableBits should be between 0 and 7. Otherwise, it is an error.
}
```

In this example, the new parameter `totalEncodedBytes` is used to compute the available bits for decoding. The decoding function uses this information to determine when to stop decoding elements of the sequence.

Please note, that for every type that is contained within a SEQUENCE OF (SOME-TYPE in our previous example), a new look ahead function must be implemented. This function (`SOME_TYPE_LA()` in our example) will be automatically generated by the compiler and will take as input the current bit stream as well as the `availableBits` and will return `true` if the element can be decoded (i.e., there are enough available bits in the bitstream) or `false` otherwise. Technical details of this function is not discussed here since it is ouf the scope of this document.

#### Encapsulation within Composite Types

There are two different cases to discuss when encapsulating a `deduced-size` type with another type. These two cases have different limitations and are discussed in the following paragraphs.

#### Case a: contained within an OCTET or BIT STRING (CONTAINING  ASN.1 syntax)

In this case, a type marked as `deduced-size` can be included inside another composite type such as SEQUENCE, CHOICE or SEQUENCE OF without any particular limitation. The encoding size in bytes is available in the parent container type by the size of OCTET or BIT STRING.

In the following example, the `deduced-size` type `MySequenceOf` is contained within a CHOICE.

```c
MYINT ::= INTEGER (0 .. 16383)
MySequenceOf ::= SEQUENCE (SIZE (0..100)) OF MYINT

MyMessage ::= CHOICE {
    a INTEGER (0 .. 200),
    b OCTET STRING (CONTAINING MySequenceOf)    -- MySequenceOf is contained within an OCTET STRING
}
```

Combined with the following ACN grammar:

```c
MySequenceOf [deduced-size]

MyMessage [] {
    a [],
    b []
}
```

This combination should compile without any issues.

#### Case b: NOT contained within an OCTET or BIT STRING

In this case, the decoding size of the `deduced-size` type is not available in the parent (container type). Therefore, the parent type becomes implicitely a `deduced-size` type. Likewise, if the parent-container type is contained in another grandparent type, without the CONTAINING ASN.1 notation, then the  grandparent type becomes also an implicit `deduced-size` type.

Let's give an example of how this will work in the case of SEQUENCE parent type.
Assume the following grammars:

```c
MYINT ::= INTEGER (0 .. 16383)
MySequenceOf ::= SEQUENCE (SIZE (0..100)) OF MYINT		-- The explicit deduced-size type. See ACN

MyMessage ::= SEQUENCE {			-- the parent SEQUENCE becomes an implicit deduced-size type
    ... ,							-- there can be as many components before the deduced-size component
    b MySequenceOf,
	crc INTEGER (0 .. 4294967295)	-- however the components after the deduced-size component must be non OPTIONAL and fixed size
}
```

Combined with the following ACN grammar:

```c
MySequenceOf [deduced-size]						-- explicitly mark this type as deduced-size

MyMessage [] {
    ... ,
    b [],
	crc [size 32]		-- FIXED SIZE
}
```
In the above case, the decoding function of MyMessage will also get the new parameter `const int totalEncodedBytes` but in this case the value of the totalEncodedBytes will refere to the total bytes of the SEQUENCE (i.e. MyMessage) and not just the SEQUENCE OF.
The decoding function will look like:

```c
flag MyMessage_ACN_Decode(MyMessage* pVal, BitStream* pBitStrm, int* pErrCode, const int totalEncodedBytes) {
  int startPos = pStream->currentPosition;
  
  //decode component before the the deduced size component

  int endPos = pStream->currentPosition;
  
  //Calculate the number of encoding bytes for MySequenceOf type.
  int MySequenceOf_totalEncodedBytes = totalEncodedBytes - (endPos - startPos)*8 - 32;
  
  if (MySequenceOf_ACN_Decode(&pVal->b, pBitStrm, pErrCode, MySequenceOf_totalEncodedBytes)) {
	
	//decode CRC 
	//...
  }

}
```

However, if the CRC component in the above example had variable encoding size, then asn1scc would not be able to calcuate the number of bytes  for the MySequenceOf component since the actual encoding size of the variable CRC component would not be known at this point. In this case, asn1scc will report a compile time error.



## Expected Benefits

By adding support for the PUS protocol, asn1scc will become more versatile and useful in space applications. This will broaden the potential user base and increase the utility of asn1scc in these environments.

## Limitations

The proposed feature cannot be used in cases where the minimum number of the encoding bits of the inner array element is less than one byte. This means that it cannot be used in the following cases:

1. In **BIT STING**s:the size of the inner element is just one bit
2. In **IA5String**s:when alphabet constraints are used that limit the encoding of each character to less than a byte.
3. In **SEQUENCE OF**s:when the inner element's minimum encoding size, as defined in ACN, is less than 1 byte.

Moreover, a `deduced-size` type cannot  be encapsulated: 
1. within a **SEQUENCE OF** when the CONTAINING ASN.1 syntax is not used (i.e. in case b)
2. within **SEQUENCE**, when the CONTAINING ASN.1 syntax is not used (i.e. in case b) and the following components are OPTIONAL or variable size.


## Conclusion

Adding support for the PUS protocol to the ACN protocol in asn1scc will improve its usability and versatility. This document serves as a roadmap for the implementation of this new feature.


