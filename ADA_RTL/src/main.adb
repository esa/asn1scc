WITH adaasn1rtl;


procedure Main with Spark_Mode is
      bs : adaasn1rtl.BitStream := adaasn1rtl.BitStream_AppendByte(100);
begin
   adaasn1rtl.BitStream_init(bs, 10);

end Main;
