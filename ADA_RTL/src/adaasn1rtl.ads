with Interfaces; 
use Interfaces;

package adaasn1rtl with Spark_Mode is


   --basic asn1scc type definitions
   type BIT is mod 2**1;
   
   subtype Asn1Byte is Interfaces.Unsigned_8;

   subtype Asn1Int is Interfaces.Integer_64;
   subtype Asn1UInt is Interfaces.Unsigned_64;
   subtype Asn1Real is Standard.Long_Float;

   subtype Asn1Boolean is Boolean;
   
   
   -- OBJECT IDENTIFIER
   OBJECT_IDENTIFIER_MAX_LENGTH : constant Integer       := 20;        -- the maximum number of components for Object Identifier
   SUBTYPE ObjectIdentifier_length_index is integer range 0..OBJECT_IDENTIFIER_MAX_LENGTH;
   SUBTYPE ObjectIdentifier_index is integer range 1..OBJECT_IDENTIFIER_MAX_LENGTH;
   type ObjectIdentifier_array is array (ObjectIdentifier_index) of Asn1UInt;

   type Asn1ObjectIdentifier is  record
      Length : ObjectIdentifier_length_index;
      values  : ObjectIdentifier_array;
   end record;
   
   
   type ASN1_RESULT is record
      Success   : Boolean;
      ErrorCode : Integer;
   end record;
   
   
   type TEST_CASE_STEP is
     (TC_VALIDATE, TC_ENCODE, TC_DECODE, TC_VALIDATE_DECODED, TC_EQUAL);

   type TEST_CASE_RESULT is record
      Step      : TEST_CASE_STEP;
      Success   : Boolean;
      ErrorCode : Integer;
   end record;
   
   subtype BIT_RANGE is Natural range 0 .. 7;
   
   
   type OctetBuffer is array (Natural range <>) of Asn1Byte;
   
   subtype OctetBuffer_0_7 is OctetBuffer (BIT_RANGE);
      
   type Bitstream  (Size_In_Bytes:Positive) is record
      Buffer           : OctetBuffer(1 .. Size_In_Bytes) ; 
      Current_Bit_Pos  : Natural;  --current bit for writing or reading in the bitsteam
   end record;
   
   
   function Sub (A : in Asn1Int; B : in Asn1Int) return Asn1UInt with
     Pre  => A >= B;
   
   function GetBytes (V : Asn1UInt) return Asn1Byte with
     Post    => GetBytes'Result >=1 and GetBytes'Result<=8;
   
   --Bit strean functions
   
   function BitStream_init (Bitstream_Size_In_Bytes : Positive) return Bitstream  with
     Pre     => Bitstream_Size_In_Bytes < Positive'Last/8,
     Post    => BitStream_init'Result.Current_Bit_Pos = 0 and BitStream_init'Result.Size_In_Bytes = Bitstream_Size_In_Bytes;
   
   
   procedure BitStream_AppendBit (bs : in out BitStream; Bit_Value : in BIT) with
     Depends => (bs => (bs, Bit_Value)),
     Pre     => bs.Current_Bit_Pos < Natural'Last and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 1;
   
   --BitStream_ReadBit
   procedure BitStream_ReadBit (bs : in out BitStream; Bit_Value : out BIT; result :    out Boolean) with
     Depends => (bs => (bs), Bit_Value => bs, result => bs),
     Pre     => bs.Current_Bit_Pos < Natural'Last and then  
                bs.Size_In_Bytes < Positive'Last/8 and then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8,
     Post    => result  and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 1;
   
   procedure BitStream_AppendByte (bs : in out BitStream; Byte_Value : in Asn1Byte; Negate : in Boolean) with
     Depends => (bs => (bs, Byte_Value, Negate)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8;
   
   
   procedure BitStream_DecodeByte (bs : in out BitStream; Byte_Value : out Asn1Byte; success   :    out Boolean) with
     Depends => (bs => (bs), Byte_Value => bs, success => bs),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 8,
     Post    => success and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8;
   
   
   
   procedure BitStream_ReadNibble (bs : in out BitStream; Byte_Value : out Asn1Byte; success   :    out Boolean) with
     Depends => (bs => (bs), Byte_Value => bs, success => bs),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 4 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - 4,
     Post    => success and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 4;
     
   
   procedure BitStream_AppendPartialByte(bs : in out BitStream; Byte_Value : in Asn1Byte; nBits : in BIT_RANGE; negate : in Boolean) with
     Depends => (bs => (bs, Byte_Value, negate, nBits)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;
     
   
   procedure BitStream_Encode_Non_Negative_Integer(bs : in out BitStream; intValue   : in Asn1UInt; nBits : in Integer) with
     Depends => (bs => (bs, intValue, nBits)),
     Pre     => nBits >= 0 and then 
                nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;
   
   procedure Enc_UInt (bs : in out BitStream;  intValue : in     Asn1UInt;  total_bytes : in     Integer) with
     Depends => (bs => (bs, intValue, total_bytes)),
     Pre     => total_bytes >= 0 and then 
                total_bytes <= Asn1UInt'Size/8 and then 
                bs.Current_Bit_Pos < Natural'Last - total_bytes*8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - total_bytes*8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes*8;

   procedure Dec_UInt (bs : in out BitStream; total_bytes : Integer; Ret: out Asn1UInt; Result :    out Boolean)  with
     Depends => (Ret => (bs,total_bytes), Result => (bs,total_bytes),  bs => (bs, total_bytes)),
     Pre     => total_bytes >= 0 and then 
                total_bytes <= Asn1UInt'Size/8 and then 
                bs.Current_Bit_Pos < Natural'Last - total_bytes*8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - total_bytes*8,
     Post    => Result and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes*8;

   
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

end adaasn1rtl;
