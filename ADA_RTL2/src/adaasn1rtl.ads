with Interfaces; 
with Ada.Characters.Latin_1;

package adaasn1rtl with Spark_Mode is

   --basic asn1scc type definitions
   type BIT is mod 2**1;
   
   subtype Asn1Byte is Interfaces.Unsigned_8;

   subtype Asn1Int is Interfaces.Integer_64;
   subtype Asn1UInt is Interfaces.Unsigned_64;
   subtype Asn1Real is Standard.Long_Float;

   subtype Asn1Boolean is Boolean;
   subtype Asn1NullType is Interfaces.Unsigned_8;
   
   type BitArray is array (Natural range <>) of BIT;
   for BitArray'Component_Size use 1;
   --pragma Pack (BitArray);
   
   type OctetBuffer is array (Natural range <>) of Asn1Byte;
   
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

   NUL : constant Standard.Character := Ada.Characters.Latin_1.NUL;

   ERR_END_OF_STREAM              : constant Integer := 1001;
   ERR_INSUFFICIENT_DATA          : constant Integer := 101;
   ERR_UNSUPPORTED_ENCODING       : constant Integer := 1002;  --  Returned when the uPER encoding for REALs is not binary encoding
   ERR_INCORRECT_STREAM           : constant Integer := 104;
   
   
   function GetZeroBasedCharIndex (CharToSearch   :    Character;   AllowedCharSet : in String) return Integer 
     with
      Pre => AllowedCharSet'First <= AllowedCharSet'Last and
      AllowedCharSet'Last <= Integer'Last - 1,
      Post =>
       (GetZeroBasedCharIndex'Result >= 0 and   GetZeroBasedCharIndex'Result <=  AllowedCharSet'Last - AllowedCharSet'First);

   function CharacterPos (C : Character) return Integer with
     Post => (CharacterPos'Result >= 0 and CharacterPos'Result <= 127);
   
   function getStringSize (str : String) return Integer with
     Pre     => str'Last < Natural'Last and then
                str'Last >= str'First, 
     Post => getStringSize'Result >= 0 and getStringSize'Result <= (str'Last - str'First + 1);
   

   function String_Equal (Left, Right : in String) return Boolean with
     Pre     => Left'Last < Natural'Last and then
                Left'Last >= Left'First  and then 
                Right'Last < Natural'Last and then
                Right'Last >= Right'First;
     
   
   
   function Asn1Real_Equal (Left, Right : in Asn1Real) return Boolean
   is (
      if Left = Right then True
      elsif Left = 0.0 then Right = 0.0
      elsif Left > Right then   ((Left - Right) / Left) < 0.00001
      else  ((Right - Left) / Left) < 0.00001
   );
   
   
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

    procedure ObjectIdentifier_Init(val:out Asn1ObjectIdentifier);
    function ObjectIdentifier_isValid(val : in Asn1ObjectIdentifier) return boolean;
    function RelativeOID_isValid(val : in Asn1ObjectIdentifier) return boolean;
    function ObjectIdentifier_equal(val1 : in Asn1ObjectIdentifier; val2 : in Asn1ObjectIdentifier) return boolean;
   
   
end adaasn1rtl;
