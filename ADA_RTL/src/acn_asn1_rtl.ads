
with adaasn1rtl;
use adaasn1rtl;


package acn_asn1_rtl with Spark_Mode is

   procedure Acn_Enc_Int_PositiveInteger_ConstSize(bs : in out BitStream;  IntVal : in     Asn1UInt;   sizeInBits   : in     Integer) with
     Depends => (bs => (bs, IntVal, sizeInBits)),
     Pre     => sizeInBits >= 0 and then 
                sizeInBits < Integer'Size and then 
                bs.Current_Bit_Pos < Natural'Last - sizeInBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - sizeInBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + sizeInBits     ;
   

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_8(bs : in out BitStream; IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16 (bs : in out BitStream; IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 16 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 16,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32 (bs : in out BitStream;  IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 32 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 32,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64 (bs : in out BitStream;   IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 64 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 64,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64;
   
   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16 (bs : in out BitStream; IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 16 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 16,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 16;
   
   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32 (bs : in out BitStream;  IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 32 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 32,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 32;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64 (bs : in out BitStream;   IntVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 64 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 64,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 64;
   
end acn_asn1_rtl;
