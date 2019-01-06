with Interfaces; 
use Interfaces;

package adaasn1rtl with Spark_Mode is

   ERR_END_OF_STREAM        : constant Integer := 1001;
   ERR_UNSUPPORTED_ENCODING : constant Integer := 1002;  --  Returned when the uPER encoding for REALs is not binary encoding
   ERR_INCORRECT_STREAM           : constant Integer := 104;
   

   --basic asn1scc type definitions
   type BIT is mod 2**1;
   
   subtype Asn1Byte is Interfaces.Unsigned_8;

   subtype Asn1Int is Interfaces.Integer_64;
   subtype Asn1UInt is Interfaces.Unsigned_64;
   subtype Asn1Real is Standard.Long_Float;

   subtype Asn1Boolean is Boolean;
   
   type Asn1Int_ARRAY_0_19 is array (0 .. 19) of Asn1UInt;
   Powers_of_10 : constant Asn1Int_ARRAY_0_19 := Asn1Int_ARRAY_0_19'(0 => 1, 1 => 10, 2 => 100, 3 => 1000, 4 => 10000, 5 => 100000, 6 => 1000000, 7 => 10000000, 8 => 100000000, 9 => 1000000000, 10 => 10000000000, 11 => 100000000000, 12 => 1000000000000, 13 => 10000000000000, 14 => 100000000000000, 15 => 1000000000000000, 16 => 10000000000000000, 17 => 100000000000000000, 18 => 1000000000000000000, 19 => 10000000000000000000);
   
   
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
   subtype OctetBuffer_16 is OctetBuffer (1 .. 16);
   subtype OctetArray4 is OctetBuffer (1 .. 4);
   
   subtype OctetBuffer_0_7 is OctetBuffer (BIT_RANGE);

   subtype Digits_Buffer is OctetBuffer (1 .. 19);
   
   type Bitstream  (Size_In_Bytes:Positive) is record
      Buffer           : OctetBuffer(1 .. Size_In_Bytes) ; 
      Current_Bit_Pos  : Natural;  --current bit for writing or reading in the bitsteam
   end record;
   
   
   function To_UInt (IntVal : Asn1Int) return Asn1UInt;
   
   --function To_Int (IntVal : Asn1UInt) return Asn1Int;
   function To_Int (IntVal : Asn1UInt) return Asn1Int is
   (
      if IntVal > Asn1UInt (Asn1Int'Last) then
          -Asn1Int (not IntVal) - 1 
      else
         Asn1Int (IntVal) );

   
   
     
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
   
   
   function Sub (A : in Asn1Int; B : in Asn1Int) return Asn1UInt with
     Pre  => A >= B;
   
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
      else 19 )
   with
     Pre => Int_value < Powers_of_10(19)   ;   
   
   
   function PLUS_INFINITY return Asn1Real;
   function MINUS_INFINITY return Asn1Real;

   function GetExponent (V : Asn1Real) return Asn1Int;
   function GetMantissa (V : Asn1Real) return Asn1UInt;
   function RequiresReverse (dummy : Boolean) return Boolean;
   
   
    procedure ObjectIdentifier_Init(val:out Asn1ObjectIdentifier);
    function ObjectIdentifier_isValid(val : in Asn1ObjectIdentifier) return boolean;
    function RelativeOID_isValid(val : in Asn1ObjectIdentifier) return boolean;
    function ObjectIdentifier_equal(val1 : in Asn1ObjectIdentifier; val2 : in Asn1ObjectIdentifier) return boolean;
   
   
   function GetZeroBasedCharIndex (CharToSearch   :    Character;   AllowedCharSet : in String) return Integer 
     with
      Pre => AllowedCharSet'First <= AllowedCharSet'Last and
      AllowedCharSet'Last <= Integer'Last - 1,
      Post =>
       (GetZeroBasedCharIndex'Result >= 0 and   GetZeroBasedCharIndex'Result <=  AllowedCharSet'Last - AllowedCharSet'First);

   function CharacterPos (C : Character) return Integer with
      Post => (CharacterPos'Result >= 0 and CharacterPos'Result <= 127);
   
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
     
   procedure BitStream_ReadPartialByte(bs : in out BitStream; Byte_Value : out Asn1Byte; nBits : in BIT_RANGE)  with
     Depends => ((bs,Byte_Value) => (bs, nBits) ),
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
   
   procedure BitStream_Decode_Non_Negative_Integer (bs : in out BitStream; IntValue : out Asn1UInt; nBits : in Integer;  result : out Boolean) with
     Depends => ((bs,IntValue, result) => (bs, nBits)),
     Pre     => nBits >= 0 and then 
                nBits < Asn1UInt'Size and then 
                bs.Current_Bit_Pos < Natural'Last - nBits and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - nBits,
     Post    => result and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + nBits;
   
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
     Post    => Result and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes*8
                and Ret >= 0 and Ret < 256**total_bytes;

   
   procedure Dec_Int (bs : in out BitStream; total_bytes : Integer; int_value: out Asn1Int; Result :    out Boolean)  with
     Depends => (int_value => (bs,total_bytes), Result => (bs,total_bytes),  bs => (bs, total_bytes)),
     Pre     => total_bytes >= 0 and then 
                total_bytes <= Asn1UInt'Size/8 and then 
                bs.Current_Bit_Pos < Natural'Last - total_bytes*8 and then  
                bs.Size_In_Bytes < Positive'Last/8 and  then
                bs.Current_Bit_Pos < bs.Size_In_Bytes * 8 - total_bytes*8,
     Post    => Result and bs.Current_Bit_Pos = bs'Old.Current_Bit_Pos + total_bytes*8;


end adaasn1rtl;
