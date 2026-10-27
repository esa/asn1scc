--  The mapping functions of the test (module test-mappers in the ACN),
--  the Ada twins of test_mappers.c
with adaasn1rtl;
use type adaasn1rtl.Asn1UInt;
use type adaasn1rtl.Asn1Int;

package Test_Mappers is
   --  plus: the wire carries the value + 2
   function plus_encode (V : adaasn1rtl.Asn1UInt) return adaasn1rtl.Asn1UInt
   is (V + 2);
   --  a corrupted wire value below 2 gives 0 (no wrap-around)
   function plus_decode (V : adaasn1rtl.Asn1UInt) return adaasn1rtl.Asn1UInt
   is (if V >= 2 then V - 2 else 0);

   --  negate: the wire carries -value
   function negate_encode (V : adaasn1rtl.Asn1Int) return adaasn1rtl.Asn1Int
   is (if V = adaasn1rtl.Asn1Int'First then V else -V);
   function negate_decode (V : adaasn1rtl.Asn1Int) return adaasn1rtl.Asn1Int
   is (if V = adaasn1rtl.Asn1Int'First then V else -V);

   --  context-length: the length is not on the wire (size 0); the decoder
   --  takes the one the encoder recorded, as a PUS parameter length known
   --  from the context
   function context_length_encode
     (V : adaasn1rtl.Asn1UInt) return adaasn1rtl.Asn1UInt;
   function context_length_decode
     (V : adaasn1rtl.Asn1UInt) return adaasn1rtl.Asn1UInt;
end Test_Mappers;
