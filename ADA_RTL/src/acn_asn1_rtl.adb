with Interfaces; 
use Interfaces;
--with uper_asn1_rtl;
--use uper_asn1_rtl;

package body acn_asn1_rtl with Spark_Mode is

   procedure Acn_Enc_Int_PositiveInteger_ConstSize  (bs : in out BitStream; IntVal : in Asn1UInt;  sizeInBits: in Integer)
   is
   begin
      BitStream_Encode_Non_Negative_Integer (bs, IntVal, sizeInBits);
   end Acn_Enc_Int_PositiveInteger_ConstSize;
   
   procedure Acn_Enc_Int_PositiveInteger_ConstSize_8(bs : in out BitStream; IntVal : in     Asn1UInt)
   is
   begin
      BitStream_AppendByte(bs, Asn1Byte(IntVal and 16#FF#) , false);
   end Acn_Enc_Int_PositiveInteger_ConstSize_8;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16 (bs : in out BitStream; IntVal : in     Asn1UInt)
   is
   begin
      Enc_UInt (bs, IntVal, 2);
   end Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32 (bs : in out BitStream;  IntVal : in     Asn1UInt)
   is
   begin
      Enc_UInt (bs, IntVal, 4);
   end Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64 (bs : in out BitStream;   IntVal : in     Asn1UInt)
   is
   begin
      Enc_UInt (bs, IntVal, 8);
   end Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64;
   

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N (bs : in out BitStream; IntVal : in     Asn1UInt; total_bytes : in     Integer) with
     Depends => (bs => (bs, IntVal, total_bytes)),
     Pre     => total_bytes >= 0 and then 
                total_bytes <= Asn1UInt'Size/8 and then 
                bs.Current_Bit_Pos < Natural'Last - total_bytes*8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - total_bytes*8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes*8
   is
      byteValue : Asn1Byte;
      tmp : Asn1UInt := IntVal;
   begin
      for i in  1..total_bytes loop
         byteValue := Asn1Byte(tmp mod 256);
         pragma Loop_Invariant (bs.Current_Bit_Pos = bs.Current_Bit_Pos'Loop_Entry + (i-1)*8);
         BitStream_AppendByte(bs, byteValue, false);
         tmp := tmp / 256;
      end loop;
   end Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N;
   
   
   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16 (bs : in out BitStream; IntVal : in     Asn1UInt)
   is
   begin
      Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(bs, IntVal, 2);
   end Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16;
   
   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32 (bs : in out BitStream; IntVal : in     Asn1UInt)
   is
   begin
      Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(bs, IntVal, 4);
   end Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32;
   
   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64 (bs : in out BitStream; IntVal : in     Asn1UInt)
   is
   begin
      Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N(bs, IntVal, 8);
   end Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64;
   

end acn_asn1_rtl;
