with Interfaces;
package Board_Config is
   subtype Asn1Int is Interfaces.Integer_64;
   subtype Asn1UInt is Interfaces.Unsigned_64;
   subtype Asn1Real is Standard.Float;
   Enumerated_Size : constant := 8;
end Board_Config;