with Interfaces;
package Board_Config is
   subtype Asn1Int is Interfaces.Integer_64;
   subtype Asn1UInt is Interfaces.Unsigned_64;
   subtype Asn1Real is Standard.Long_Float;
   Enumerated_Size : constant := 32;
end Board_Config;