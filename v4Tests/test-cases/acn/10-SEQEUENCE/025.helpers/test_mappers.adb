package body Test_Mappers is
   Context_Length : adaasn1rtl.Asn1UInt := 0;

   function context_length_encode
     (V : adaasn1rtl.Asn1UInt) return adaasn1rtl.Asn1UInt
   is
   begin
      Context_Length := V;
      return 0;
   end context_length_encode;

   function context_length_decode
     (V : adaasn1rtl.Asn1UInt) return adaasn1rtl.Asn1UInt
   is
      pragma Unreferenced (V);
   begin
      return Context_Length;
   end context_length_decode;
end Test_Mappers;
