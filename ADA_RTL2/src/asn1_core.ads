with Interfaces; 
use Interfaces;

package asn1_core with Spark_Mode is

   --basic asn1scc type definitions
   type BIT is mod 2**1;
   
   subtype Asn1Byte is Interfaces.Unsigned_8;

   subtype Asn1Int is Interfaces.Integer_64;
   subtype Asn1UInt is Interfaces.Unsigned_64;
   subtype Asn1Real is Standard.Long_Float;

   subtype Asn1Boolean is Boolean;
   subtype Asn1NullType is Interfaces.Unsigned_8;
   
   
   -- OBJECT IDENTIFIER
   OBJECT_IDENTIFIER_MAX_LENGTH : constant Integer       := 20;        -- the maximum number of components for Object Identifier
   SUBTYPE ObjectIdentifier_length_index is integer range 0..OBJECT_IDENTIFIER_MAX_LENGTH;
   SUBTYPE ObjectIdentifier_index is integer range 1..OBJECT_IDENTIFIER_MAX_LENGTH;
   type ObjectIdentifier_array is array (ObjectIdentifier_index) of Asn1UInt;

   type Asn1ObjectIdentifier is  record
      Length : ObjectIdentifier_length_index;
      values  : ObjectIdentifier_array;
   end record;
   

end asn1_core;
