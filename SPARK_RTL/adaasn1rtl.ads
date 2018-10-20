with Ada.Characters.Latin_1;
with Interfaces; use Interfaces;
use type Interfaces.Integer_64;

--    inherit Ada.Characters.Latin_1,
--            Interfaces;

package adaasn1rtl with
     Spark_Mode is
   subtype RANGE_1_2 is Natural range 1 .. 2;
   subtype RANGE_1_4 is Natural range 1 .. 4;
   subtype RANGE_1_8 is Natural range 1 .. 8;
   subtype RANGE_1_16 is Natural range 1 .. 16;
   subtype RANGE_1_100 is Natural range 1 .. 100;
   subtype RANGE_1_1K is Natural range 1 .. 1024;

   type BIT is mod 2**1;
   type BitArray is array (Natural range <>) of BIT;
   for BitArray'Component_Size use 1;
   pragma Pack (BitArray);

   subtype Asn1Byte is Interfaces.Unsigned_8;
   type OctetBuffer is array (Natural range <>) of Asn1Byte;

   subtype Asn1Int is Interfaces.Integer_64;
   subtype Asn1UInt is Interfaces.Unsigned_64;
   subtype Asn1Real is Standard.Long_Float;

   subtype Asn1Boolean is Boolean;
   subtype OctetBuffer_8 is OctetBuffer (RANGE_1_8);
   subtype OctetBuffer_16 is OctetBuffer (RANGE_1_16);
   subtype Asn1NullType is Interfaces.Unsigned_8;

   subtype OctetArray2 is OctetBuffer (RANGE_1_2);
   subtype OctetArray4 is OctetBuffer (RANGE_1_4);
   subtype OctetArray8 is OctetBuffer (RANGE_1_8);
   subtype OctetArray100 is OctetBuffer (RANGE_1_100);
   subtype OctetArray1K is OctetBuffer (RANGE_1_1K);

    OBJECT_IDENTIFIER_MAX_LENGTH : constant Integer       := 20;        -- the maximum number of components for Object Identifier
    SUBTYPE ObjectIdentifier_length_index is integer range 0..OBJECT_IDENTIFIER_MAX_LENGTH;
    SUBTYPE ObjectIdentifier_index is integer range 1..OBJECT_IDENTIFIER_MAX_LENGTH;
    type ObjectIdentifier_array is array (ObjectIdentifier_index) of Asn1UInt;

    type Asn1ObjectIdentifier is  record
        Length : ObjectIdentifier_length_index;
        values  : ObjectIdentifier_array;
    end record;



   function PLUS_INFINITY return Asn1Real;
   function MINUS_INFINITY return Asn1Real;

   function GetExponent (V : Asn1Real) return Asn1Int;
   function GetMantissa (V : Asn1Real) return Asn1UInt;
   function RequiresReverse (dummy : Boolean) return Boolean;

   function Asn1Real_Equal (Left, Right : in Asn1Real) return Boolean;
   function Asn1Boolean_Equal (Left, Right : in Asn1Boolean) return Boolean;
   function Asn1Int_Equal (Left, Right : in Asn1Int) return Boolean;
   function Asn1NullType_Equal (Left, Right : in Asn1NullType) return Boolean;

   function UInt16_to_OctetArray2
     (x : Interfaces.Unsigned_16) return OctetArray2;
   function OctetArray2_to_UInt16
     (x : OctetArray2) return Interfaces.Unsigned_16;

   function UInt32_to_OctetArray4
     (x : Interfaces.Unsigned_32) return OctetArray4;
   function OctetArray4_to_UInt32
     (x : OctetArray4) return Interfaces.Unsigned_32;

   function Asn1UInt_to_OctetArray8 (x : Asn1UInt) return OctetArray8;
   function OctetArray8_to_Asn1UInt (x : OctetArray8) return Asn1UInt;

   function Int16_to_OctetArray2
     (x : Interfaces.Integer_16) return OctetArray2;
   function OctetArray2_to_Int16
     (x : OctetArray2) return Interfaces.Integer_16;

   function Int32_to_OctetArray4
     (x : Interfaces.Integer_32) return OctetArray4;
   function OctetArray4_to_Int32
     (x : OctetArray4) return Interfaces.Integer_32;

   function Asn1Int_to_OctetArray8 (x : Asn1Int) return OctetArray8;
   function OctetArray8_to_Asn1Int (x : OctetArray8) return Asn1Int;

   function Float_to_OctetArray4 (x : Float) return OctetArray4;
   function Long_Float_to_OctetArray8 (x : Asn1Real) return OctetArray8;
   function OctetArray4_to_Float (x : OctetArray4) return Float;
   function OctetArray8_to_Long_Float (x : OctetArray8) return Asn1Real;

   NUL : constant Standard.Character := Ada.Characters.Latin_1.NUL;

   ERR_INCORRECT_DATA             : constant Integer := 105;
   ERR_INSUFFICIENT_DATA          : constant Integer := 101;
   ERR_INCORRECT_PER_STREAM       : constant Integer := 102;
   ERR_INVALID_CHOICE_ALTERNATIVE : constant Integer := 103;
   ERR_INCORRECT_STREAM           : constant Integer := 104;
   ERR_INVALID_BER_FILE           : constant Integer := 201;
   ERR_BER_LENGTH_MISMATCH        : constant Integer := 202;
   ERR_BER_TAG_MISMATCH           : constant Integer := 203;

   type DECODE_PARAMS is record
      K       : Natural;
      DataLen : Natural;
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

   function getStringSize (str : String) return Integer with
      Pre => str'First = 1 and
      str'Last >= str'First and
      str'Last <= Integer'Last - 1,
      Post => (getStringSize'Result >= 0 and getStringSize'Result <= str'Last);

   function stringContainsChar
     (str : String;
      ch  : Character) return Boolean with
      Pre => str'Last <= Integer'Last - 1;

   function GetZeroBasedCharIndex
     (CharToSearch   :    Character;
      AllowedCharSet : in String) return Integer with
      Pre => AllowedCharSet'Last >= AllowedCharSet'First and
      AllowedCharSet'Last <= Integer'Last - 1,
      Post =>
      (GetZeroBasedCharIndex'Result >= 0 and
       GetZeroBasedCharIndex'Result <=
         AllowedCharSet'Last - AllowedCharSet'First);

   function CharacterPos (C : Character) return Integer with
      Post => (CharacterPos'Result >= 0 and CharacterPos'Result <= 127);

   procedure BitStream_AppendBit
     (S      : in out BitArray;
      I      : in out Natural;
      BitVal : in     BIT) with
      Depends => (I => I, S => (S, BitVal, I)),
      Pre     => I >= S'First - 1 and I <= S'Last - 1,
      Post    => I = I'Old + 1;

   procedure BitStream_ReadBit
     (S      : in     BitArray;
      P      : in out DECODE_PARAMS;
      BitVal :    out BIT;
      result :    out Boolean) with
      Depends => (BitVal => (P, S), P => P, result => (P)),
      Pre     => P.K >= S'First - 1 and P.K <= S'Last - 1,
      Post    => P.K = P'Old.K + 1;

   procedure BitStream_AppendByte
     (S         : in out BitArray;
      K         : in out Natural;
      ByteValue : in     Asn1Byte;
      Negate    : in     Boolean) with
      Depends => (K => K, S => (S, ByteValue, K, Negate)),
      Pre     => K + 1 >= S'First and K + 8 <= S'Last,
      Post    => K = K'Old + 8;

   procedure BitStream_DecodeByte
     (S         : in     BitArray;
      P         : in out DECODE_PARAMS;
      ByteValue :    out Asn1Byte;
      success   :    out Boolean) with
      Depends => (P => P, ByteValue => (S, P), success => (P)),
      Pre     => P.K + 1 >= S'First and P.K + 8 <= S'Last,
      Post    => P.K = P'Old.K + 8;

   procedure BitStream_ReadNibble
     (S         : in     BitArray;
      P         : in out DECODE_PARAMS;
      ByteValue :    out Asn1Byte;
      success   :    out Boolean) with
      Depends => (P => (P), ByteValue => (S, P), success => (P)),
      Pre     => P.K + 1 >= S'First and P.K + 4 <= S'Last,
      Post    => P.K = P'Old.K + 4;

   procedure BitStream_AppendPartialByte
     (S         : in out BitArray;
      K         : in out Natural;
      ByteValue : in     Asn1Byte;
      NBits     : in     Integer;
      Negate    : in     Boolean) with
      Depends => (K => (K, NBits), S => (S, ByteValue, K, NBits, Negate)),
      Pre     => NBits >= 1 and
      NBits <= 7 and
      K + 1 >= S'First and
      K + NBits <= S'Last and
      S'First + NBits - 1 <= S'Last,
      Post => K = K'Old + NBits;

   procedure BitStream_Encode_Non_Negative_Integer
     (S          : in out BitArray;
      K          : in out Natural;
      IntValue   : in     Asn1UInt;
      nBitsRange : in     Integer) with
      Depends => (K => (K, nBitsRange), S => (S, IntValue, K, nBitsRange)),
      Pre     => nBitsRange >= 0 and
      nBitsRange <= 64 and
      K + 1 >= S'First and
      K + nBitsRange <= S'Last,
      Post => K = K'Old + nBitsRange;

   procedure UPER_Enc_SemiConstraintWholeNumber
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int;
      MinVal : in     Asn1Int) with
      Depends => ((K) => (IntVal, K, MinVal), (S) => (S, IntVal, K, MinVal)),
      Pre => IntVal - MinVal >= 0 and K + 1 >= S'First and K + 72 <= S'Last,
      Post    => K >= K'Old + 16 and K <= K'Old + 72;

   procedure UPER_Dec_SemiConstraintWholeNumber
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      MinVal : in     Asn1Int;
      Result :    out Boolean) with
      Depends => ((IntVal, Result) => (K, MinVal, S), K => (K, S)),
      Pre     => K.K + 1 >= S'First and K.K + 72 <= S'Last,
      Post => K.K >= K'Old.K + 8 and K.K <= K'Old.K + 72 and IntVal >= MinVal;

   procedure UPER_Enc_SemiConstraintPosWholeNumber
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt;
      MinVal : in     Asn1UInt) with
      Depends => ((K) => (IntVal, K, MinVal), (S) => (S, IntVal, K, MinVal)),
      Pre => IntVal - MinVal >= 0 and K + 1 >= S'First and K + 72 <= S'Last,
      Post    => K >= K'Old + 16 and K <= K'Old + 72;

   procedure UPER_Dec_SemiConstraintPosWholeNumber
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      MinVal : in     Asn1UInt;
      Result :    out Boolean) with
      Depends => ((IntVal, Result) => (K, MinVal, S), K => (K, S)),
      Pre     => K.K + 1 >= S'First and K.K + 72 <= S'Last,
      Post => K.K >= K'Old.K + 8 and K.K <= K'Old.K + 72 and IntVal >= MinVal;

   procedure UPER_Enc_ConstraintWholeNumber
     (S           : in out BitArray;
      K           : in out Natural;
      IntVal      : in     Asn1Int;
      MinVal      : in     Asn1Int;
      nSizeInBits : in     Integer) with
      Depends =>
      (K => (K, nSizeInBits),
       S => (S, IntVal, K, MinVal, nSizeInBits)),
      Pre => IntVal >= MinVal and
      nSizeInBits >= 0 and
      nSizeInBits <= 64 and
      K + 1 >= S'First and
      K + nSizeInBits <= S'Last,
      Post => K = K'Old + nSizeInBits;

   procedure UPER_Dec_ConstraintWholeNumber
     (S           : in     BitArray;
      K           : in out DECODE_PARAMS;
      IntVal      :    out Asn1Int;
      MinVal      : in     Asn1Int;
      MaxVal      : in     Asn1Int;
      nSizeInBits : in     Integer;
      Result      :    out Boolean) with
      Depends =>
      ((IntVal, Result) => (K, MaxVal, MinVal, nSizeInBits, S),
       K                => (K, nSizeInBits)),
      Pre => nSizeInBits >= 0 and
      nSizeInBits <= 64 and
      K.K + 1 >= S'First and
      K.K + nSizeInBits <= S'Last,
      Post => K.K = K'Old.K + nSizeInBits and
      (Result = True
       and then (((IntVal >= MinVal) and (IntVal <= MaxVal)))) and
      (Result = False and then (IntVal = MinVal));

   procedure UPER_Enc_ConstraintPosWholeNumber
     (S           : in out BitArray;
      K           : in out Natural;
      IntVal      : in     Asn1UInt;
      MinVal      : in     Asn1UInt;
      nSizeInBits : in     Integer) with
      Depends =>
      (K => (K, nSizeInBits),
       S => (S, IntVal, K, MinVal, nSizeInBits)),
      Pre => IntVal >= MinVal and
      nSizeInBits >= 0 and
      nSizeInBits <= 64 and
      K + 1 >= S'First and
      K + nSizeInBits <= S'Last,
      Post => K = K'Old + nSizeInBits;

   procedure UPER_Dec_ConstraintPosWholeNumber
     (S           : in     BitArray;
      K           : in out DECODE_PARAMS;
      IntVal      :    out Asn1UInt;
      MinVal      : in     Asn1UInt;
      MaxVal      : in     Asn1UInt;
      nSizeInBits : in     Integer;
      Result      :    out Boolean) with
      Depends =>
      ((IntVal, Result) => (K, MaxVal, MinVal, nSizeInBits, S),
       K                => (K, nSizeInBits)),
      Pre => nSizeInBits >= 0 and
      nSizeInBits <= 64 and
      K.K + 1 >= S'First and
      K.K + nSizeInBits <= S'Last,
      Post => K.K = K'Old.K + nSizeInBits and
      (Result = True
       and then (((IntVal >= MinVal) and (IntVal <= MaxVal)))) and
      (Result = False and then (IntVal = MinVal));

   procedure UPER_Dec_ConstraintWholeNumberInt
     (S           : in     BitArray;
      K           : in out DECODE_PARAMS;
      IntVal      :    out Integer;
      MinVal      : in     Integer;
      MaxVal      : in     Integer;
      nSizeInBits : in     Integer;
      Result      :    out Boolean) with
      Depends =>
      ((IntVal, Result) => (K, MaxVal, MinVal, nSizeInBits, S),
       K                => (K, nSizeInBits)),
      Pre => nSizeInBits >= 0 and
      nSizeInBits <= 64 and
      K.K + 1 >= S'First and
      K.K + nSizeInBits <= S'Last,
      Post => K.K = K'Old.K + nSizeInBits and
      (Result = True
       and then (((IntVal >= MinVal) and (IntVal <= MaxVal)))) and
      (Result = False and then (IntVal = MinVal));

   procedure UPER_Enc_UnConstraintWholeNumber
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int) with
      Depends => ((K) => (IntVal, K), (S) => (S, IntVal, K)),
      Pre     => K + 1 >= S'First and K + 72 <= S'Last,
      Post    => K >= K'Old + 16 and K <= K'Old + 72;

   procedure UPER_Dec_UnConstraintWholeNumber
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      Result :    out Boolean) with
      Depends => ((IntVal, K, Result) => (K, S)),
      Pre     => K.K + 1 >= S'First and K.K + 72 <= S'Last,
      Post    => K.K >= K'Old.K + 8 and K.K <= K'Old.K + 72;

   procedure UPER_Dec_UnConstraintWholeNumberMax
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      MaxVal : in     Asn1Int;
      Result :    out Boolean) with
      Depends => ((IntVal, Result) => (K, MaxVal, S), K => (K, S)),
      Pre     => K.K + 1 >= S'First and K.K + 72 <= S'Last,
      Post => K.K >= K'Old.K + 8 and K.K <= K'Old.K + 72 and IntVal <= MaxVal;

   procedure UPER_Enc_Boolean
     (S   : in out BitArray;
      I   : in out Natural;
      Val : in     Asn1Boolean) with
      Depends => (I => (I), S => (S, Val, I)),
      Pre     => I >= S'First - 1 and I <= S'Last - 1,
      Post    => I = I'Old + 1;

   procedure UPER_Dec_boolean
     (S      : in     BitArray;
      P      : in out DECODE_PARAMS;
      val    :    out Asn1Boolean;
      result :    out Boolean) with
      Depends => (val => (P, S), P => (P), result => (P)),
      Pre     => P.K >= S'First - 1 and P.K <= S'Last - 1,
      Post    => P.K = P'Old.K + 1;

   procedure UPER_Enc_Real
     (S       : in out BitArray;
      K       : in out Natural;
      RealVal : in     Asn1Real) with
      Depends => (S => (S, RealVal, K), K => (RealVal, K)),
      Pre     => K + 1 >= S'First and K + 104 <= S'Last,
      Post    => K >= K'Old and K <= K'Old + 104;

   procedure UPER_Dec_Real
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      RealVal :    out Asn1Real;
      Result  :    out ASN1_RESULT) with
      Depends => (K => (S, K), RealVal => (S, K), Result => (S, K)),
      Pre     => K.K + 1 >= S'First and K.K + 104 <= S'Last,
      Post    => K.K >= K'Old.K and K.K <= K'Old.K + 104;


    procedure ObjectIdentifier_Init(val:out Asn1ObjectIdentifier);
    function ObjectIdentifier_isValid(val : in Asn1ObjectIdentifier) return boolean;
    function RelativeOID_isValid(val : in Asn1ObjectIdentifier) return boolean;

    function ObjectIdentifier_equal(val1 : in Asn1ObjectIdentifier; val2 : in Asn1ObjectIdentifier) return boolean;
    procedure ObjectIdentifier_uper_encode(S : in out BitArray;  K : in out Natural; val : Asn1ObjectIdentifier);
    procedure ObjectIdentifier_uper_decode(S : in     BitArray;  K : in out DECODE_PARAMS;  val :    out Asn1ObjectIdentifier;  Result  :    out ASN1_RESULT);

    procedure RelativeOID_uper_encode(S : in out BitArray;  K : in out Natural; val : Asn1ObjectIdentifier);
    procedure RelativeOID_uper_decode(S : in     BitArray;  K : in out DECODE_PARAMS;  val :    out Asn1ObjectIdentifier;  Result  :    out ASN1_RESULT);

   procedure Acn_Enc_Int_PositiveInteger_ConstSize
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt;
      Size   : in     Integer) with
      Depends => (K => (K, Size), S => (S, IntVal, K, Size)),
      Pre     => IntVal >= 0 and
      Size >= 0 and
      Size <= 64 and
      IntVal <= 2**Size - 1 and
      K + 1 >= S'First and
      K + Size <= S'Last,
      Post => K = K'Old + Size;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_8
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= 0 and
      IntVal <= 255 and
      K + 1 >= S'First and
      K + 8 <= S'Last,
      Post => K = K'Old + 8;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= 0 and
      IntVal <= 65535 and
      K + 1 >= S'First and
      K + 16 <= S'Last,
      Post => K = K'Old + 16;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= 0 and
      IntVal <= 4294967295 and
      K + 1 >= S'First and
      K + 32 <= S'Last,
      Post => K = K'Old + 32;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= 0 and K + 1 >= S'First and K + 64 <= S'Last,
      Post    => K = K'Old + 64;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= 0 and
      IntVal <= 65535 and
      K + 1 >= S'First and
      K + 16 <= S'Last,
      Post => K = K'Old + 16;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= 0 and
      IntVal <= 4294967295 and
      K + 1 >= S'First and
      K + 32 <= S'Last,
      Post => K = K'Old + 32;

   procedure Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= 0 and K + 1 >= S'First and K + 64 <= S'Last,
      Post    => K = K'Old + 64;

   procedure Acn_Enc_Int_PositiveInteger_VarSize_LengthEmbedded
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt) with
      Depends => ((K) => (IntVal, K), (S) => (S, IntVal, K)),
      Pre     => IntVal >= 0 and K + 1 >= S'First and K + 72 <= S'Last,
      Post    => K >= K'Old + 16 and K <= K'Old + 72;

   procedure Acn_Enc_Int_TwosComplement_ConstSize
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int;
      Size   : in     Integer) with
      Depends => (K => (K, Size), S => (S, IntVal, K, Size)),
      Pre     => IntVal >= -2**(Size - 1) and
      Size >= 0 and
      Size <= 64 and
      IntVal <= 2**(Size - 1) - 1 and
      K + 1 >= S'First and
      K + Size <= S'Last,
      Post => K = K'Old + Size;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_8
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= -128 and
      IntVal <= 127 and
      K + 1 >= S'First and
      K + 8 <= S'Last,
      Post => K = K'Old + 8;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_16
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= -32768 and
      IntVal <= 32767 and
      K + 1 >= S'First and
      K + 16 <= S'Last,
      Post => K = K'Old + 16;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_32
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= -2147483648 and
      IntVal <= 2147483647 and
      K + 1 >= S'First and
      K + 32 <= S'Last,
      Post => K = K'Old + 32;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_big_endian_64
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= -9223372036854775808 and
      IntVal <= 9223372036854775807 and
      K + 1 >= S'First and
      K + 64 <= S'Last,
      Post => K = K'Old + 64;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_16
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= -32768 and
      IntVal <= 32767 and
      K + 1 >= S'First and
      K + 16 <= S'Last,
      Post => K = K'Old + 16;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_32
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= -2147483648 and
      IntVal <= 2147483647 and
      K + 1 >= S'First and
      K + 32 <= S'Last,
      Post => K = K'Old + 32;

   procedure Acn_Enc_Int_TwosComplement_ConstSize_little_endian_64
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int) with
      Depends => (K => (K), S => (S, IntVal, K)),
      Pre     => IntVal >= -9223372036854775808 and
      IntVal <= 9223372036854775807 and
      K + 1 >= S'First and
      K + 64 <= S'Last,
      Post => K = K'Old + 64;

   procedure Acn_Enc_Int_TwosComplement_VarSize_LengthEmbedded
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int) with
      Depends => ((K) => (K, IntVal), (S) => (S, K, IntVal)),
      Pre     => K + 1 >= S'First and K + 72 <= S'Last,
      Post    => K >= K'Old + 16 and K <= K'Old + 72;

   procedure Acn_Enc_Int_BCD_ConstSize
     (S        : in out BitArray;
      K        : in out Natural;
      IntVal   : in     Asn1UInt;
      nNibbles : in     Integer) with
      Depends => (K => (K, nNibbles), S => (S, IntVal, K, nNibbles)),
      Pre     => IntVal >= 0 and
      nNibbles >= 0 and
      nNibbles <= 18 and
      IntVal <= 10**nNibbles - 1 and
      K + 1 >= S'First and
      K + 4 * nNibbles <= S'Last,
      Post => K = K'Old + 4 * nNibbles;

   procedure Acn_Enc_Int_BCD_VarSize_LengthEmbedded
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt) with
      Depends => (K => (K, IntVal), S => (S, IntVal, K)),
      Pre     => IntVal >= 0 and K + 1 >= S'First and K + 88 <= S'Last,
      Post    => K >= K'Old and K <= K'Old + 88;

   procedure Acn_Enc_Int_BCD_VarSize_NullTerminated
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt) with
      Depends => (K => (K, IntVal), S => (S, IntVal, K)),
      Pre     => IntVal >= 0 and K + 1 >= S'First and K + 76 <= S'Last,
      Post    => K >= K'Old and K <= K'Old + 76;

   procedure Acn_Enc_Int_ASCII_ConstSize
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int;
      nChars : in     Integer) with
      Depends => (K => (K, nChars), S => (S, IntVal, K, nChars)),
      Pre     => nChars >= 0 and
      nChars <= 18 and
      IntVal >= -(10**(nChars - 1) - 1) and
      IntVal <= 10**(nChars - 1) - 1 and
      IntVal >= -99999999999999999 and
      IntVal <= 99999999999999999 and
      K + 1 >= S'First and
      K + 8 * nChars <= S'Last,
      Post => K = K'Old + 8 * nChars;

   procedure Acn_Enc_UInt_ASCII_ConstSize
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt;
      nChars : in     Integer) with
      Depends => (K => (K, nChars), S => (S, IntVal, K, nChars)),
      Pre     => nChars >= 0 and
      nChars <= 18 and
      IntVal >= 0 and
      IntVal <= 10**(nChars) - 1 and
      IntVal >= 0 and
      IntVal <= 999999999999999999 and
      K + 1 >= S'First and
      K + 8 * nChars <= S'Last,
      Post => K = K'Old + 8 * nChars;

   procedure Acn_Enc_Int_ASCII_VarSize_LengthEmbedded
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int) with
      Depends => (K => (K, IntVal), S => (S, IntVal, K)),
      Pre     => K + 1 >= S'First and K + 160 <= S'Last,
      Post    => K >= K'Old and K <= K'Old + 160;

   procedure Acn_Enc_Int_ASCII_VarSize_NullTerminated
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1Int;
      nullChar : in Asn1Byte) with
      Depends => (K => (K, IntVal), S => (S, IntVal, K, nullChar)),
      Pre     => K + 1 >= S'First and K + 168 <= S'Last,
      Post    => K >= K'Old and K <= K'Old + 168;

   procedure Acn_Enc_UInt_ASCII_VarSize_LengthEmbedded
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt) with
      Depends => (K => (K, IntVal), S => (S, IntVal, K)),
      Pre     => K + 1 >= S'First and K + 168 <= S'Last,
      Post    => K >= K'Old and K <= K'Old + 168;

   procedure Acn_Enc_UInt_ASCII_VarSize_NullTerminated
     (S      : in out BitArray;
      K      : in out Natural;
      IntVal : in     Asn1UInt;
      nullChar : in Asn1Byte) with
      Depends => (K => (K, IntVal), S => (S, IntVal, K, nullChar)),
      Pre     => K + 1 >= S'First and K + 168 <= S'Last,
      Post    => K >= K'Old and K <= K'Old + 168;

   procedure Acn_Dec_Int_PositiveInteger_ConstSize
     (S           : in     BitArray;
      K           : in out DECODE_PARAMS;
      IntVal      :    out Asn1UInt;
      minVal      : in     Asn1UInt;
      maxVal      : in     Asn1UInt;
      nSizeInBits : in     Integer;
      Result      :    out ASN1_RESULT) with
      Depends =>
      (IntVal => (K, nSizeInBits, S, minVal, maxVal),
       Result => (K, nSizeInBits, S, minVal, maxVal),
       K      => (K, nSizeInBits)),
      Pre => nSizeInBits >= 0 and
      nSizeInBits <= 64 and
      K.K + 1 >= S'First and
      K.K + nSizeInBits <= S'Last,
      Post => K.K = K'Old.K + nSizeInBits and
      (Result.Success = True
       and then (((IntVal >= minVal) and (IntVal <= maxVal)))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_8
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 8 <= S'Last,
      Post    => K.K = K'Old.K + 8 and
      (Result.Success = True
       and then (((IntVal >= minVal) and (IntVal <= maxVal)))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 16 <= S'Last,
      Post    => K.K = K'Old.K + 16 and
      (Result.Success = True
       and then (((IntVal >= minVal) and (IntVal <= maxVal)))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 32 <= S'Last,
      Post    => K.K = K'Old.K + 32 and
      (Result.Success = True
       and then (((IntVal >= minVal) and (IntVal <= maxVal)))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 64 <= S'Last,
      Post    => K.K = K'Old.K + 64 and
      (Result.Success = True
       and then (((IntVal >= minVal) and (IntVal <= maxVal)))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 16 <= S'Last,
      Post    => K.K = K'Old.K + 16 and
      (Result.Success = True
       and then (((IntVal >= minVal) and (IntVal <= maxVal)))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 32 <= S'Last,
      Post    => K.K = K'Old.K + 32 and
      (Result.Success = True
       and then (((IntVal >= minVal) and (IntVal <= maxVal)))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 64 <= S'Last,
      Post    => K.K = K'Old.K + 64 and
      (Result.Success = True
       and then (((IntVal >= minVal) and (IntVal <= maxVal)))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal), K => (K, S)),
      Pre     => K.K + 1 >= S'First and K.K + 72 <= S'Last,
      Post    => K.K >= K'Old.K and
      K.K <= K'Old.K + 72 and
      (Result.Success = True and then (IntVal >= minVal)) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize
     (S           : in     BitArray;
      K           : in out DECODE_PARAMS;
      IntVal      :    out Asn1Int;
      minVal      : in     Asn1Int;
      maxVal      : in     Asn1Int;
      nSizeInBits : in     Integer;
      Result      :    out ASN1_RESULT) with
      Depends =>
      (IntVal => (K, nSizeInBits, S, minVal, maxVal),
       Result => (K, nSizeInBits, S, minVal, maxVal),
       K      => (K, nSizeInBits)),
      Pre => nSizeInBits >= 0 and
      nSizeInBits <= 64 and
      K.K + 1 >= S'First and
      K.K + nSizeInBits <= S'Last and
      -2**(nSizeInBits - 1) <= minVal and
      maxVal <= 2**(nSizeInBits - 1) - 1,
      Post => K.K = K'Old.K + nSizeInBits and
      (Result.Success = True
       and then (((IntVal >= minVal)) and (IntVal <= maxVal))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_8
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and
      K.K + 8 <= S'Last and
      -128 <= minVal and
      maxVal <= 127,
      Post => K.K = K'Old.K + 8 and
      (Result.Success = True
       and then (((IntVal >= minVal)) and (IntVal <= maxVal))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and
      K.K + 16 <= S'Last and
      -32768 <= minVal and
      maxVal <= 32767,
      Post => K.K = K'Old.K + 16 and
      (Result.Success = True
       and then (((IntVal >= minVal)) and (IntVal <= maxVal))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and
      K.K + 32 <= S'Last and
      -2147483648 <= minVal and
      maxVal <= 2147483647,
      Post => K.K = K'Old.K + 32 and
      (Result.Success = True
       and then (((IntVal >= minVal)) and (IntVal <= maxVal))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 64 <= S'Last,
      Post    => K.K = K'Old.K + 64 and
      (Result.Success = True
       and then (((IntVal >= minVal)) and (IntVal <= maxVal))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and
      K.K + 16 <= S'Last and
      -32768 <= minVal and
      maxVal <= 32767,
      Post => K.K = K'Old.K + 16 and
      (Result.Success = True
       and then (((IntVal >= minVal)) and (IntVal <= maxVal))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and
      K.K + 32 <= S'Last and
      -2147483648 <= minVal and
      maxVal <= 2147483647,
      Post => K.K = K'Old.K + 32 and
      (Result.Success = True
       and then (((IntVal >= minVal)) and (IntVal <= maxVal))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 64 <= S'Last,
      Post    => K.K = K'Old.K + 64 and
      (Result.Success = True
       and then (((IntVal >= minVal)) and (IntVal <= maxVal))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S), K => (K, S)),
      Pre     => K.K + 1 >= S'First and K.K + 72 <= S'Last,
      Post    => K.K >= K'Old.K and K.K <= K'Old.K + 72;

   procedure Acn_Dec_Int_BCD_ConstSize
     (S        : in     BitArray;
      K        : in out DECODE_PARAMS;
      IntVal   :    out Asn1UInt;
      minVal   : in     Asn1UInt;
      maxVal   : in     Asn1UInt;
      nNibbles : in     Integer;
      Result   :    out ASN1_RESULT) with
      Depends =>
      ((IntVal, Result) => (K, S, nNibbles, minVal, maxVal),
       K                => (K, S, nNibbles)),
      Pre => K.K + 1 >= S'First and
      K.K + 4 * nNibbles <= S'Last and
      nNibbles >= 0 and
      nNibbles <= 18 and
      minVal >= 0 and
      maxVal <= 10**nNibbles - 1,
      Post => K.K >= K'Old.K and
      K.K <= K'Old.K + 4 * nNibbles and
      (Result.Success = True
       and then (((IntVal >= minVal) and (IntVal <= maxVal)))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_BCD_VarSize_LengthEmbedded
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 88 <= S'Last,
      Post    => K.K >= K'Old.K and
      K.K <= K'Old.K + 88 and
      (Result.Success = True and then (IntVal >= 0)) and
      (Result.Success = False and then (IntVal = 0));

   procedure Acn_Dec_Int_BCD_VarSize_NullTerminated
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, minVal, maxVal), K => (K, S)),
      Pre     => K.K + 1 >= S'First and K.K + 76 <= S'Last,
      Post    => K.K >= K'Old.K and
      K.K <= K'Old.K + 76 and
      (Result.Success = True
       and then (((IntVal >= minVal) and (IntVal <= maxVal)))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_ASCII_ConstSize
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      minVal : in     Asn1Int;
      maxVal : in     Asn1Int;
      nChars : in     Integer;
      Result :    out ASN1_RESULT) with
      Depends =>
      ((IntVal, Result) => (K, S, nChars, minVal, maxVal),
       K                => (K, S, nChars)),
      Pre => K.K + 1 >= S'First and
      K.K + 8 * nChars <= S'Last and
      nChars >= 1 and
      nChars <= 19,
      Post => K.K >= K'Old.K and
      K.K <= K'Old.K + 8 * nChars and
      (Result.Success = True
       and then (((IntVal >= minVal) and (IntVal <= maxVal)))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_UInt_ASCII_ConstSize
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      minVal : in     Asn1UInt;
      maxVal : in     Asn1UInt;
      nChars : in     Integer;
      Result :    out ASN1_RESULT) with
      Depends =>
      ((IntVal, Result) => (K, S, nChars, minVal, maxVal),
       K                => (K, S, nChars)),
      Pre => K.K + 1 >= S'First and
      K.K + 8 * nChars <= S'Last and
      nChars >= 1 and
      nChars <= 19,
      Post => K.K >= K'Old.K and
      K.K <= K'Old.K + 8 * nChars and
      (Result.Success = True
       and then (((IntVal >= minVal) and (IntVal <= maxVal)))) and
      (Result.Success = False and then (IntVal = minVal));

   procedure Acn_Dec_Int_ASCII_VarSize_LengthEmbedded
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 168 <= S'Last,
      Post    => K.K >= K'Old.K and
      K.K <= K'Old.K + 168 and
      (Result.Success = True and then (((IntVal >= 0)))) and
      (Result.Success = False and then (IntVal = 0));

   procedure Acn_Dec_Int_ASCII_VarSize_NullTerminated
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1Int;
      nullChar:   in Asn1Byte;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, nullChar), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 168 <= S'Last,
      Post    => K.K >= K'Old.K and
      K.K <= K'Old.K + 168 and
      (Result.Success = True and then (((IntVal >= 0)))) and
      (Result.Success = False and then (IntVal = 0));
   procedure Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 168 <= S'Last,
      Post    => K.K >= K'Old.K and
      K.K <= K'Old.K + 168 and
      (Result.Success = True and then (((IntVal >= 0)))) and
      (Result.Success = False and then (IntVal = 0));

   procedure Acn_Dec_UInt_ASCII_VarSize_NullTerminated
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      IntVal :    out Asn1UInt;
      nullChar:   in Asn1Byte;
      Result :    out ASN1_RESULT) with
      Depends => ((IntVal, Result) => (K, S, nullChar), K => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 168 <= S'Last,
      Post    => K.K >= K'Old.K and
      K.K <= K'Old.K + 168 and
      (Result.Success = True and then (((IntVal >= 0)))) and
      (Result.Success = False and then (IntVal = 0));

   procedure Acn_Enc_Real_IEEE754_32_big_endian
     (S       : in out BitArray;
      K       : in out Natural;
      RealVal : in     Asn1Real) with
      Depends => (S => (S, RealVal, K), K => (K)),
      Pre     => K + 1 >= S'First and K + 32 <= S'Last,
      Post    => K = K'Old + 32;

   procedure Acn_Dec_Real_IEEE754_32_big_endian
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      RealVal :    out Asn1Real;
      Result  :    out ASN1_RESULT) with
      Depends => (K => (K), RealVal => (S, K), Result => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 32 <= S'Last,
      Post    => K.K >= K'Old.K and K.K <= K'Old.K + 32;

   procedure Acn_Enc_Real_IEEE754_64_big_endian
     (S       : in out BitArray;
      K       : in out Natural;
      RealVal : in     Asn1Real) with
      Depends => (S => (S, RealVal, K), K => (K)),
      Pre     => K + 1 >= S'First and K + 64 <= S'Last,
      Post    => K = K'Old + 64;

   procedure Acn_Dec_Real_IEEE754_64_big_endian
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      RealVal :    out Asn1Real;
      Result  :    out ASN1_RESULT) with
      Depends => (K => (K), RealVal => (S, K), Result => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 64 <= S'Last,
      Post    => K.K >= K'Old.K and K.K <= K'Old.K + 64;

   procedure Acn_Enc_Real_IEEE754_32_little_endian
     (S       : in out BitArray;
      K       : in out Natural;
      RealVal : in     Asn1Real) with
      Depends => (S => (S, RealVal, K), K => (K)),
      Pre     => K + 1 >= S'First and K + 32 <= S'Last,
      Post    => K = K'Old + 32;

   procedure Acn_Dec_Real_IEEE754_32_little_endian
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      RealVal :    out Asn1Real;
      Result  :    out ASN1_RESULT) with
      Depends => (K => (K), RealVal => (S, K), Result => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 32 <= S'Last,
      Post    => K.K >= K'Old.K and K.K <= K'Old.K + 32;

   procedure Acn_Enc_Real_IEEE754_64_little_endian
     (S       : in out BitArray;
      K       : in out Natural;
      RealVal : in     Asn1Real) with
      Depends => (S => (S, RealVal, K), K => (K)),
      Pre     => K + 1 >= S'First and K + 64 <= S'Last,
      Post    => K = K'Old + 64;

   procedure Acn_Dec_Real_IEEE754_64_little_endian
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      RealVal :    out Asn1Real;
      Result  :    out ASN1_RESULT) with
      Depends => (K => (K), RealVal => (S, K), Result => (K)),
      Pre     => K.K + 1 >= S'First and K.K + 64 <= S'Last,
      Post    => K.K >= K'Old.K and K.K <= K'Old.K + 64;

   procedure Acn_Enc_Boolean_true_pattern
     (S       : in out BitArray;
      K       : in out Natural;
      BoolVal : in     Asn1Boolean;
      pattern : in     BitArray) with
      Depends => (S => (S, BoolVal, K, pattern), K => (K, pattern)),
      Pre     => K + 1 >= S'First and K + pattern'Length <= S'Last,
      Post    => K = K'Old + pattern'Length;

   procedure Acn_Dec_Boolean_true_pattern
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      BoolVal :    out Asn1Boolean;
      pattern : in     BitArray;
      Result  :    out ASN1_RESULT) with
      Depends =>
      (K       => (K, pattern),
       BoolVal => (S, K, pattern),
       Result  => (K, pattern)),
      Pre  => K.K + 1 >= S'First and K.K + pattern'Length <= S'Last,
      Post => K.K = K'Old.K + pattern'Length;

   procedure Acn_Enc_Boolean_false_pattern
     (S       : in out BitArray;
      K       : in out Natural;
      BoolVal : in     Asn1Boolean;
      pattern : in     BitArray) with
      Depends => (S => (S, BoolVal, K, pattern), K => (K, pattern)),
      Pre     => K + 1 >= S'First and K + pattern'Length <= S'Last,
      Post    => K = K'Old + pattern'Length;

   procedure Acn_Dec_Boolean_false_pattern
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      BoolVal :    out Asn1Boolean;
      pattern : in     BitArray;
      Result  :    out ASN1_RESULT) with
      Depends =>
      (K       => (K, pattern),
       BoolVal => (S, K, pattern),
       Result  => (K, pattern)),
      Pre  => K.K + 1 >= S'First and K.K + pattern'Length <= S'Last,
      Post => K.K = K'Old.K + pattern'Length;

   procedure Acn_Enc_NullType_pattern
     (S       : in out BitArray;
      K       : in out Natural;
      encVal  : in     Asn1NullType;
      pattern : in     BitArray) with
      Depends => (S => (S, K, encVal, pattern), K => (K, pattern)),
      Pre     => K + 1 >= S'First and K + pattern'Length <= S'Last,
      Post    => K = K'Old + pattern'Length;

   procedure Acn_Dec_NullType_pattern
     (S        : in     BitArray;
      K        : in out DECODE_PARAMS;
      decValue :    out Asn1NullType;
      pattern  : in     BitArray;
      Result   :    out ASN1_RESULT) with
      Depends =>
      (K        => (K, pattern),
       Result   => (K, pattern, S),
       decValue => (S, K, pattern)),
      Pre  => K.K + 1 >= S'First and K.K + pattern'Length <= S'Last,
      Post => K.K = K'Old.K + pattern'Length;

   procedure Acn_Enc_NullType_pattern2
     (S       : in out BitArray;
      K       : in out Natural;
      pattern : in     BitArray) with
      Depends => (S => (S, K, pattern), K => (K, pattern)),
      Pre     => K + 1 >= S'First and K + pattern'Length <= S'Last,
      Post    => K = K'Old + pattern'Length;

   procedure Acn_Dec_NullType_pattern2
     (S       : in     BitArray;
      K       : in out DECODE_PARAMS;
      pattern : in     BitArray;
      Result  :    out ASN1_RESULT) with
      Depends => (K => (K, pattern), Result => (K, pattern, S)),
      Pre     => K.K + 1 >= S'First and K.K + pattern'Length <= S'Last,
      Post    => K.K = K'Old.K + pattern'Length;

   procedure Acn_Enc_NullType
     (S      : in out BitArray;
      K      : in out Natural;
      encVal : in     Asn1NullType) with
      Depends => (S => (S, K, encVal), K => (K)),
      Pre     => K + 1 >= S'First and K <= S'Last,
      Post    => K = K'Old;

   procedure Acn_Dec_NullType
     (S        : in     BitArray;
      K        : in out DECODE_PARAMS;
      decValue :    out Asn1NullType;
      Result   :    out ASN1_RESULT) with
      Depends => (K => (K), Result => (K), decValue => (S, K)),
      Pre     => K.K + 1 >= S'First and K.K <= S'Last,
      Post    => K.K = K'Old.K;

      -- String functions
   procedure Acn_Enc_String_Ascii_FixSize
     (S      : in out BitArray;
      K      : in out Natural;
      strVal : in     String) with
      Depends => (S => (S, K, strVal), K => (K, strVal)),
      Pre     => K + 1 >= S'First and K + 8 * (strVal'Length - 1) <= S'Last,
      Post    => K = K'Old + 8 * (strVal'Length - 1);

   procedure Acn_Dec_String_Ascii_FixSize
     (S      : in     BitArray;
      K      : in out DECODE_PARAMS;
      strVal : in out String;
      Result :    out ASN1_RESULT) with
      Depends =>
      (K      => (S, K, strVal),
       strVal => (S, K, strVal),
       Result => (S, K, strVal)),
      Pre => S'First = 1 and
      K.K + 1 >= S'First and
      K.K + 8 * (strVal'Length - 1) <= S'Last,
      Post => K.K = K'Old.K + 8 * (strVal'Length - 1);

   procedure Acn_Enc_String_Ascii_Null_Teminated
     (S              : in out BitArray;
      K              : in out Natural;
      null_character : in     Integer;
      strVal         : in     String);
   procedure Acn_Dec_String_Ascii_Null_Teminated
     (S              : in     BitArray;
      K              : in out DECODE_PARAMS;
      null_character : in     Integer;
      strVal         : in out String;
      Result         :    out ASN1_RESULT);

   procedure Acn_Enc_String_Ascii_Internal_Field_Determinant
     (S                            : in out BitArray;
      K                            : in out Natural;
      asn1Min                      :        Asn1Int;
      nLengthDeterminantSizeInBits : in     Integer;
      strVal                       : in     String);
   procedure Acn_Dec_String_Ascii_Internal_Field_Determinant
     (S                            : in     BitArray;
      K                            : in out DECODE_PARAMS;
      asn1Min                      :        Asn1Int;
      asn1Max                      :        Asn1Int;
      nLengthDeterminantSizeInBits : in     Integer;
      strVal                       : in out String;
      Result                       :    out ASN1_RESULT);

   procedure Acn_Enc_String_Ascii_External_Field_Determinant
     (S      : in out BitArray;
      K      : in out Natural;
      strVal : in     String);
   procedure Acn_Dec_String_Ascii_External_Field_Determinant
     (S                    : in     BitArray;
      K                    : in out DECODE_PARAMS;
      extSizeDeterminatFld : in     Asn1Int;
      strVal               : in out String;
      Result               :    out ASN1_RESULT);

   procedure Acn_Enc_String_CharIndex_FixSize
     (S         : in out BitArray;
      K         : in out Natural;
      charSet   :        String;
      nCharSize :        Integer;
      strVal    : in     String);
   procedure Acn_Dec_String_CharIndex_FixSize
     (S         : in     BitArray;
      K         : in out DECODE_PARAMS;
      charSet   :        String;
      nCharSize :        Integer;
      strVal    : in out String;
      Result    :    out ASN1_RESULT);

   procedure Acn_Enc_String_CharIndex_External_Field_Determinant
     (S         : in out BitArray;
      K         : in out Natural;
      charSet   :        String;
      nCharSize :        Integer;
      strVal    : in     String);
   procedure Acn_Dec_String_CharIndex_External_Field_Determinant
     (S                    : in     BitArray;
      K                    : in out DECODE_PARAMS;
      charSet              :        String;
      nCharSize            :        Integer;
      extSizeDeterminatFld : in     Asn1Int;
      strVal               : in out String;
      Result               :    out ASN1_RESULT);

   procedure Acn_Enc_String_CharIndex_Internal_Field_Determinant
     (S                            : in out BitArray;
      K                            : in out Natural;
      charSet                      :        String;
      nCharSize                    :        Integer;
      asn1Min                      :        Asn1Int;
      nLengthDeterminantSizeInBits : in     Integer;
      strVal                       : in     String);
   procedure Acn_Dec_String_CharIndex_Internal_Field_Determinant
     (S                            : in     BitArray;
      K                            : in out DECODE_PARAMS;
      charSet                      :        String;
      nCharSize                    :        Integer;
      asn1Min                      :        Asn1Int;
      asn1Max                      :        Asn1Int;
      nLengthDeterminantSizeInBits : in     Integer;
      strVal                       : in out String;
      Result                       :    out ASN1_RESULT);

   function milbus_encode (IntVal : in Asn1Int) return Asn1Int;
   function milbus_decode (IntVal : in Asn1Int) return Asn1Int;

end adaasn1rtl;
