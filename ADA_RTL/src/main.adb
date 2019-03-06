WITH adaasn1rtl;
with acn_asn1_rtl;


procedure Main with Spark_Mode is
      bs : adaasn1rtl.BitStream := adaasn1rtl.BitStream_init(100);
begin
--   adaasn1rtl.BitStream_AppendByte(bs, 10, false);
   acn_asn1_rtl.Acn_Enc_Boolean_true_pattern(bs,true,adaasn1rtl.BitArray'(1,1,1,1));
   acn_asn1_rtl.Acn_Enc_String_Ascii_Null_Teminated2(bs, adaasn1rtl.OctetBuffer'(234,45,0,12),"asdvasd");

end Main;
