with Interfaces; 
use Interfaces;
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
   
end adaasn1rtl;
