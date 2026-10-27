# ACN mapping functions

An ACN integer can carry a `mapping-function`: the encoder writes `f_encode(value)`, the decoder reads the wire value
and takes `f_decode(wire)`. Typical uses are the CCSDS packet data length (octets − 1) and lengths that are not on the
wire at all (`size 0`): the decode function supplies them from the context.

```
MyPDU [] {
    len INTEGER [encoding pos-int, size 16, mapping-function my-mappers.data-length],
    data [size len]
}
```

The function name `data-length` becomes `data_length_encode` / `data_length_decode` (and a module name `my-mappers`
becomes `my_mappers`). The user writes both functions; asn1scc only calls them.

## C

```c
asn1SccUint data_length_encode(asn1SccUint value);
asn1SccUint data_length_decode(asn1SccUint value);
```

The generated `.c` file declares the functions it calls (`extern`). With a module (`mapping-function my-mappers.f` or
`-mfm my_mappers`) it also does `#include "my_mappers.h"`. The type is the C type of the field (`asn1SccUint` for an ACN
`INTEGER` field with an unsigned encoding).

## Ada

The functions live in a package, which the code needs: name it in the ACN (`mapping-function my-mappers.f`) or give it
with `-mfm <Package>` (`--mapping-functions-module`). Without one, asn1scc stops with an error at the
`mapping-function` (before 4.9.62.0 it generated code that did not compile).

```ada
with adaasn1rtl;

package My_Mappers is
   function data_length_encode (V : adaasn1rtl.Asn1UInt) return adaasn1rtl.Asn1UInt;
   function data_length_decode (V : adaasn1rtl.Asn1UInt) return adaasn1rtl.Asn1UInt;
end My_Mappers;
```

- The functions take and return the integer type of the runtime that the field's encoding reads and writes:
  `adaasn1rtl.Asn1UInt` for the unsigned encodings (`pos-int`, BCD, ASCII of an unsigned value), `adaasn1rtl.Asn1Int`
  for the signed ones (`twos-complement`, ASCII). The wire value need not be a value of the field (`size 0` gives 0, an
  offset mapping may leave the field's range), so the decoder reads it into a temporary of that type.
- After `f_decode` the decoder checks the range of the field (only the bounds narrower than the runtime type) and fails
  with the field's error code when the value is outside; the field gets the value only when it is inside.
- `f_decode` is called with any value the encoding can carry, also from a damaged stream (the automatic tests decode
  corrupted copies): it should not raise an exception for such a value (e.g. no wrap-around check failure).
- The generated packages are `SPARK_Mode`; for `gnatprove` the mapping package must be SPARK as well.

## --acn-v2

With `--acn-v2` a length determinant whose consumer is in another function is reserved first and patched later
(`Acn_PatchDet_*`). The patch writes the mapped value, as the field's own encoder does: `f_encode(length)`, also for the
fallback value written when no consumer was encoded (before 4.9.62.0 the patch wrote the raw length).
