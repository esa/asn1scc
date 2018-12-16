WITH adaasn1rtl;


procedure Main with Spark_Mode is
      bs : adaasn1rtl.BitStream := adaasn1rtl.BitStream_init(100);
begin
   adaasn1rtl.BitStream_AppendByte(bs, 10, false);

end Main;
