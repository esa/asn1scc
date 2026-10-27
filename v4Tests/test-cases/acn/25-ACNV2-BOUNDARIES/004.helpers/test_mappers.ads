--  The mapping functions of the tests (module test-mappers in the ACN),
--  the Ada twins of plus_mapping.c
with adaasn1rtl;
use type adaasn1rtl.Asn1UInt;

package Test_Mappers is
   --  mapping-function plus: the wire carries the length + 2
   function plus_encode (V : adaasn1rtl.Asn1UInt) return adaasn1rtl.Asn1UInt
   is (V + 2);
   --  a corrupted wire value below 2 gives 0 (no wrap-around)
   function plus_decode (V : adaasn1rtl.Asn1UInt) return adaasn1rtl.Asn1UInt
   is (if V >= 2 then V - 2 else 0);
end Test_Mappers;
