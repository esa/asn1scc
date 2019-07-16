with Interfaces;
use Interfaces;

package adaasn1rtl.encoding with Spark_Mode is

   


   type Asn1Int_ARRAY_0_19 is array (0 .. 19) of Asn1UInt;
   Powers_of_10 : constant Asn1Int_ARRAY_0_19 := Asn1Int_ARRAY_0_19'(0 => 1, 1 => 10, 2 => 100, 3 => 1000, 4 => 10000, 5 => 100000, 6 => 1000000, 7 => 10000000, 8 => 100000000, 9 => 1000000000, 10 => 10000000000, 11 => 100000000000, 12 => 1000000000000, 13 => 10000000000000, 14 => 100000000000000, 15 => 1000000000000000, 16 => 10000000000000000, 17 => 100000000000000000, 18 => 1000000000000000000, 19 => 10000000000000000000);

   
   
   subtype BIT_RANGE is Natural range 0 .. 7;
   
   
   subtype OctetBuffer_16 is OctetBuffer (1 .. 16);
   subtype OctetArray4 is OctetBuffer (1 .. 4);
   subtype OctetArray8 is OctetBuffer (1 .. 8);
   
   subtype OctetBuffer_0_7 is OctetBuffer (BIT_RANGE);

   subtype Digits_Buffer is OctetBuffer (1 .. 20);
   
   type Bitstream  (Size_In_Bytes:Positive) is record
      Buffer           : OctetBuffer(1 .. Size_In_Bytes) ; 
      Current_Bit_Pos  : Natural;  --current bit for writing or reading in the bitsteam
   end record;
   
   type BitstreamPtr  is record
      Size_In_Bytes    :   Positive;
      Current_Bit_Pos  : Natural;  --current bit for writing or reading in the bitsteam
   end record;

   
   function RequiresReverse  return Boolean;
   function Long_Float_to_Float (x : Asn1Real) return Float;

   function To_UInt (IntVal : Asn1Int) return Asn1UInt;
   
   --function To_Int (IntVal : Asn1UInt) return Asn1Int;
   function To_Int (IntVal : Asn1UInt) return Asn1Int is
   (
      if IntVal > Asn1UInt (Asn1Int'Last) then
          -Asn1Int (not IntVal) - 1 
      else
       Asn1Int (IntVal) );
   
   function abs_value(intVal: Asn1Int) return Asn1UInt is
     (
      if intVal >= 0  then Asn1Uint(intVal) else (Asn1Uint(-(intVal+1))+1)
     );

   
   -- In some cases, SPARK cannot prove the following function
   -- This seems to be a bug since the function is proved if comment
   -- some irrelevant code e.g. the getStringSize function
     
   function To_Int_n (IntVal : Asn1UInt; nBits : Integer) return Asn1Int
   is (
       if IntVal > Asn1UInt(Shift_Left(Asn1UInt(1), nBits - 1) - 1) then  -- is given value greater than the maximum pos value in nBits space?
          --Asn1Int ( ((not (Shift_Left(Asn1UInt(1), nBits) - 1)) or IntVal)) -- in this case the number is negative ==> prefix with 1111 
          -Asn1Int ( not( (not (Shift_Left(Asn1UInt(1), nBits) - 1)) or IntVal)) - 1 -- in this case the number is negative ==> prefix with 1111 
       else
          Asn1Int (IntVal)
      )
     with 
       Pre => nBits > 0 and nBits < Asn1UInt'Size;
   
   function OctetString_equal(len1 : in Integer;len2 : in Integer; arr1 : in OctetBuffer; arr2 : in OctetBuffer) return boolean
   is
   (
      len1 = len2 and then arr1(arr1'First .. arr1'First + (len1 - 1)) = arr2(arr2'First .. arr2'First + (len1 - 1))
   )
       with 
         Pre => len1 > 0 and len2 > 0 and arr1'First + (len1-1) <= arr1'Last and arr2'First + (len2-1) <= arr1'Last;
   

   function BitString_equal(len1 : in Integer;len2 : in Integer; arr1 : in BitArray; arr2 : in BitArray) return boolean
   is
   (
      len1 = len2 and then arr1(arr1'First .. arr1'First + (len1 - 1)) = arr2(arr2'First .. arr2'First + (len1 - 1))
   )
       with 
         Pre => len1 > 0 and len2 > 0 and arr1'First + (len1-1) <= arr1'Last and arr2'First + (len2-1) <= arr1'Last;
   
   function Sub (A : in Asn1Int; B : in Asn1Int) return Asn1UInt with
     Pre  => A >= B;
   
   function stringContainsChar (str : String; ch : Character) return Boolean;
   
   function GetBytes (V : Asn1UInt) return Asn1Byte with
     Post    => GetBytes'Result >=1 and GetBytes'Result<=8;
   
   function GetLengthInBytesOfSInt (V : Asn1Int) return Asn1Byte with
     Post    => GetLengthInBytesOfSInt'Result >=1 and GetLengthInBytesOfSInt'Result<=8;
   
     
   function Get_number_of_digits (Int_value : Asn1UInt) return Integer 
   is (
      if Int_value < Powers_of_10(1) then 1
      elsif Int_value >= Powers_of_10(1) and Int_value < Powers_of_10(2)    then 2 
      elsif Int_value >= Powers_of_10(2) and Int_value < Powers_of_10(3)    then 3 
      elsif Int_value >= Powers_of_10(3) and Int_value < Powers_of_10(4)    then 4 
      elsif Int_value >= Powers_of_10(4) and Int_value < Powers_of_10(5)    then 5 
      elsif Int_value >= Powers_of_10(5) and Int_value < Powers_of_10(6)    then 6 
      elsif Int_value >= Powers_of_10(6) and Int_value < Powers_of_10(7)    then 7 
      elsif Int_value >= Powers_of_10(7) and Int_value < Powers_of_10(8)    then 8 
      elsif Int_value >= Powers_of_10(8) and Int_value < Powers_of_10(9)    then 9 
      elsif Int_value >= Powers_of_10(9) and Int_value < Powers_of_10(10)   then 10 
      elsif Int_value >= Powers_of_10(10) and Int_value < Powers_of_10(11)  then 11 
      elsif Int_value >= Powers_of_10(11) and Int_value < Powers_of_10(12)  then 12 
      elsif Int_value >= Powers_of_10(12) and Int_value < Powers_of_10(13)  then 13 
      elsif Int_value >= Powers_of_10(13) and Int_value < Powers_of_10(14)  then 14 
      elsif Int_value >= Powers_of_10(14) and Int_value < Powers_of_10(15)  then 15 
      elsif Int_value >= Powers_of_10(15) and Int_value < Powers_of_10(16)  then 16 
      elsif Int_value >= Powers_of_10(16) and Int_value < Powers_of_10(17)  then 17 
      elsif Int_value >= Powers_of_10(17) and Int_value < Powers_of_10(18)  then 18 
      elsif Int_value >= Powers_of_10(18) and Int_value < Powers_of_10(19)  then 19 
      else 20 )
      ;   
   
   
   function Asn1Real_Equal (Left, Right : in Asn1Real) return Boolean
   is (
      if Left = Right then True
      elsif Left = 0.0 then Right = 0.0
      elsif Left > Right then   ((Left - Right) / Left) < 0.00001
      else  ((Right - Left) / Left) < 0.00001
   );
   
   
   
   function PLUS_INFINITY return Asn1Real;
   function MINUS_INFINITY return Asn1Real;

   function GetExponent (V : Asn1Real) return Asn1Int;
   function GetMantissa (V : Asn1Real) return Asn1UInt;
   function RequiresReverse (dummy : Boolean) return Boolean;
   
   
    procedure ObjectIdentifier_Init(val:out Asn1ObjectIdentifier);
    function ObjectIdentifier_isValid(val : in Asn1ObjectIdentifier) return boolean;
    function RelativeOID_isValid(val : in Asn1ObjectIdentifier) return boolean;
    function ObjectIdentifier_equal(val1 : in Asn1ObjectIdentifier; val2 : in Asn1ObjectIdentifier) return boolean;
   
   
   
   function milbus_encode (IntVal : in Asn1Int) return Asn1Int 
   is ( if IntVal = 32 then 0  else IntVal);

   function milbus_decode (IntVal : in Asn1Int) return Asn1Int 
   is (if IntVal = 0 then 32   else IntVal);
   
   
   
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
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8;
   
   
   procedure BitStream_DecodeByte (bs : in out BitStream; Byte_Value : out Asn1Byte; success   :    out Boolean) with
     Depends => (bs => (bs), Byte_Value => bs, success => bs),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 8,
     Post    => success and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 8;
   
   
   
   procedure BitStream_ReadNibble (bs : in out BitStream; Byte_Value : out Asn1Byte; success   :    out Boolean) with
     Depends => (bs => (bs), Byte_Value => bs, success => bs),
     Pre     => bs.Current_Bit_Pos < Natural'Last - 4 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - 4,
     Post    => success and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + 4;
     
   
   procedure BitStream_AppendPartialByte(bs : in out BitStream; Byte_Value : in Asn1Byte; nBits : in BIT_RANGE; negate : in Boolean) with
     Depends => (bs => (bs, Byte_Value, negate, nBits)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;
     
   procedure BitStream_ReadPartialByte(bs : in out BitStream; Byte_Value : out Asn1Byte; nBits : in BIT_RANGE)  with
     Depends => ((bs,Byte_Value) => (bs, nBits) ),
     Pre     => bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;   
   
   procedure BitStream_Encode_Non_Negative_Integer(bs : in out BitStream; intValue   : in Asn1UInt; nBits : in Integer) with
     Depends => (bs => (bs, intValue, nBits)),
     Pre     => nBits >= 0 and then 
                nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;
   
   procedure BitStream_Decode_Non_Negative_Integer (bs : in out BitStream; IntValue : out Asn1UInt; nBits : in Integer;  result : out Boolean) with
     Depends => ((bs,IntValue, result) => (bs, nBits)),
     Pre     => nBits >= 0 and then 
                nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => result and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;
   
   procedure Enc_UInt (bs : in out BitStream;  intValue : in     Asn1UInt;  total_bytes : in     Integer) with
     Depends => (bs => (bs, intValue, total_bytes)),
     Pre     => total_bytes >= 0 and then 
                total_bytes <= Asn1UInt'Size/8 and then 
                bs.Current_Bit_Pos < Natural'Last - total_bytes*8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - total_bytes*8,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes*8;
   
   

   procedure Dec_UInt (bs : in out BitStream; total_bytes : Integer; Ret: out Asn1UInt; Result :    out Boolean)  with
     Depends => (Ret => (bs,total_bytes), Result => (bs,total_bytes),  bs => (bs, total_bytes)),
     Pre     => total_bytes >= 0 and then 
                total_bytes <= Asn1UInt'Size/8 and then 
                bs.Current_Bit_Pos < Natural'Last - total_bytes*8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - total_bytes*8,
     Post    => Result and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes*8
                and Ret >= 0 and Ret < 256**total_bytes;

   
   procedure Dec_Int (bs : in out BitStream; total_bytes : Integer; int_value: out Asn1Int; Result :    out Boolean)  with
     Depends => (int_value => (bs,total_bytes), Result => (bs,total_bytes),  bs => (bs, total_bytes)),
     Pre     => total_bytes >= 0 and then 
                total_bytes <= Asn1UInt'Size/8 and then 
                bs.Current_Bit_Pos < Natural'Last - total_bytes*8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - total_bytes*8,
     Post    => Result and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes*8;

    pragma Inline (BitStream_AppendByte);
   

   
   procedure Enc_SemiConstraintWholeNumber (bs : in out BitStream; IntVal : in Asn1Int; MinVal : in     Asn1Int) with
     Depends => (bs => (bs, IntVal, MinVal)),
     Pre     => IntVal >= MinVal and then
                bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);
   
   procedure Dec_SemiConstraintWholeNumber (bs : in out BitStream; IntVal : out Asn1Int; MinVal : in  Asn1Int;  Result :    out Boolean) with
      Depends => ((IntVal, Result) => (bs, MinVal), bs => (bs, MinVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8) and IntVal >= MinVal ;
   
   procedure Enc_SemiConstraintPosWholeNumber (bs : in out BitStream; IntVal : in Asn1UInt; MinVal : in     Asn1UInt) with
     Depends => (bs => (bs, IntVal, MinVal)),
     Pre     => IntVal >= MinVal and then
                bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);  
   
   procedure Dec_SemiConstraintPosWholeNumber (bs : in out BitStream; IntVal : out Asn1UInt; MinVal : in     Asn1UInt; Result :    out Boolean)  with
     Depends => ((IntVal, Result) => (bs, MinVal), bs => (bs, MinVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8) and IntVal >= MinVal ;   
   
   procedure Enc_UnConstraintWholeNumber (bs : in out BitStream; IntVal : in Asn1Int)  with
     Depends => (bs => (bs, IntVal)),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);  


   procedure Dec_UnConstraintWholeNumber (bs : in out BitStream; IntVal :    out Asn1Int; Result :    out Boolean) with
     Depends => ((IntVal, bs, Result) => bs),
     Pre     => bs.Current_Bit_Pos < Natural'Last - (Asn1UInt'Size + 8) and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - (Asn1UInt'Size + 8),
     Post    => bs.Current_Bit_Pos >= bs'Old.Current_Bit_Pos and bs.Current_Bit_Pos <= bs'Old.Current_Bit_Pos + (Asn1UInt'Size + 8);  
   
   
   procedure Enc_ConstraintWholeNumber (bs : in out BitStream; IntVal : in Asn1Int; MinVal : in Asn1Int; nBits : in Integer) with
     Depends => (bs => (bs, IntVal, MinVal, nBits)),
     Pre     => IntVal >= MinVal and then
                nBits >= 0 and then nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;
   
   procedure Enc_ConstraintPosWholeNumber (bs : in out BitStream; IntVal: in Asn1UInt; MinVal : in Asn1UInt; nBits : in Integer) with
     Depends => (bs => (bs, IntVal, MinVal, nBits)),
     Pre     => IntVal >= MinVal and then
                nBits >= 0 and then nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;
   
   procedure Dec_ConstraintWholeNumber (bs : in out BitStream; IntVal : out Asn1Int; MinVal : in Asn1Int; MaxVal : in Asn1Int; nBits : in Integer; Result : out Boolean) with
     Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal, nBits)),
     Pre     => MinVal <= MaxVal and then
                nBits >= 0 and then nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
                (
                     (Result   and  (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
                     (not Result  and  (IntVal = MinVal))
                );
   
   procedure Dec_ConstraintPosWholeNumber (bs : in out BitStream; IntVal : out Asn1UInt; MinVal : in Asn1UInt; MaxVal : in Asn1UInt; nBits : in Integer; Result : out Boolean) with
     Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal, nBits)),
     Pre     => MinVal <= MaxVal and then
                nBits >= 0 and then nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
                (
                     (Result   and  (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
                     (not Result  and  (IntVal = MinVal))
                );
   
      procedure Dec_ConstraintWholeNumberInt
     (bs : in out BitStream;
      IntVal      :    out Integer;
      MinVal      : in     Integer;
      MaxVal      : in     Integer;
      nBits : in     Integer;
      Result      :    out Boolean) with
     Depends => ((bs, IntVal, Result) => (bs, MinVal, MaxVal, nBits)),
     Pre     => MinVal <= MaxVal and then
                nBits >= 0 and then nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos <= bs.Size_In_Bytes * 8 - nBits,
     Post    => bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits and
                (
                     (Result   and  (((IntVal >= MinVal) and (IntVal <= MaxVal)))) or
                     (not Result  and  (IntVal = MinVal))
                );

   
end adaasn1rtl.encoding;
