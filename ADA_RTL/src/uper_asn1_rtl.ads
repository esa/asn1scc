with Interfaces; 
use Interfaces;
with adaasn1rtl;
use adaasn1rtl;


package uper_asn1_rtl with Spark_Mode is

   --UPER encode functions 
   procedure UPER_Enc_SemiConstraintWholeNumber (bs : in out BitStream; IntVal : in Asn1Int; MinVal : in     Asn1Int) with
     Depends => (bs => (bs, IntVal, MinVal)),
     Pre     => IntVal >= MinVal and then
                bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);
   
   procedure UPER_Dec_SemiConstraintWholeNumber (bs : in out BitStream; IntVal : out Asn1Int; MinVal : in  Asn1Int;  Result :    out Boolean) with
      Depends => ((IntVal, Result) => (bs, MinVal), bs => (bs, MinVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8) and IntVal >= MinVal ;
   
   procedure UPER_Enc_SemiConstraintPosWholeNumber (bs : in out BitStream; IntVal : in Asn1UInt; MinVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal, MinVal)),
     Pre     => IntVal >= MinVal and then
                bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);  
   
   procedure UPER_Dec_SemiConstraintPosWholeNumber (bs : in out BitStream; IntVal : out Asn1UInt; MinVal : in     Asn1UInt; Result :    out Boolean)  with
     Depends => ((IntVal, Result) => (bs, MinVal), bs => (bs, MinVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8) and IntVal >= MinVal ;   
   

end uper_asn1_rtl;
